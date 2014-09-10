{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SimpleDB.Monadic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon SimpleDB is a highly available and flexible non-relational data
-- store that offloads the work of database administration. Developers simply
-- store and query data items via web services requests and Amazon SimpleDB
-- does the rest. Unbound by the strict requirements of a relational database,
-- Amazon SimpleDB is optimized to provide high availability and flexibility,
-- with little or no administrative burden. Behind the scenes, Amazon SimpleDB
-- creates and manages multiple geographically distributed replicas of your
-- data automatically to enable high availability and data durability. The
-- service charges you only for the resources actually consumed in storing
-- your data and serving your requests. You can change your data model on the
-- fly, and data is automatically indexed for you. With Amazon SimpleDB, you
-- can focus on application development without worrying about infrastructure
-- provisioning, high availability, software maintenance, schema and index
-- management, or performance tuning.
--
-- This module is provided for convenience. It offers an alternative to the
-- common idiom of supplying required fields to an operations's smart constructor,
-- using the operation's lenses to modify additional fields, and then sending
-- or paginating the request.
--
-- As an example: using "Network.AWS.SimpleDB" with the smart constructor and
-- basic lens syntax, before explicitly calling 'send':
--
-- @
-- import Control.Monad.Trans.AWS
-- import Network.AWS.SimpleDB
--
-- send $ (mkOperationName w x)
--      & onLensField1 .~ y
--      & onLensField2 .~ z
-- @
--
-- Versus using "Network.AWS.SimpleDB.Monadic" with the 'State' operator variants from
-- "Control.Lens.Setter" such as '.=' to modify any additional request
-- parameters before sending:
--
-- @
-- import Network.AWS.SimpleDB.Monadic
--
-- operationName w x $ do
--     onLensField1 .= y
--     onLensField2 .= z
-- @
--
module Network.AWS.SimpleDB.Monadic
    (
    -- * BatchDeleteAttributes
    -- $BatchDeleteAttributes
      batchDeleteAttributes
    , batchDeleteAttributesCatch

    -- * BatchPutAttributes
    -- $BatchPutAttributes
    , batchPutAttributes
    , batchPutAttributesCatch

    -- * CreateDomain
    -- $CreateDomain
    , createDomain
    , createDomainCatch

    -- * DeleteAttributes
    -- $DeleteAttributes
    , deleteAttributes
    , deleteAttributesCatch

    -- * DeleteDomain
    -- $DeleteDomain
    , deleteDomain
    , deleteDomainCatch

    -- * DomainMetadata
    -- $DomainMetadata
    , domainMetadata
    , domainMetadataCatch

    -- * GetAttributes
    -- $GetAttributes
    , getAttributes
    , getAttributesCatch

    -- * ListDomains
    -- $ListDomains
    , listDomains
    , listDomainsCatch

    -- * PutAttributes
    -- $PutAttributes
    , putAttributes
    , putAttributesCatch

    -- * Select
    -- $Select
    , select
    , selectCatch

    -- * Re-exported
    , module Network.AWS.SimpleDB

    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.SimpleDB

type ServiceEr = Er SimpleDB

-- $BatchDeleteAttributes
-- Performs multiple DeleteAttributes operations in a single call, which
-- reduces round trips and latencies. This enables Amazon SimpleDB to optimize
-- requests, which generally yields better throughput. If you specify
-- BatchDeleteAttributes without attributes or values, all the attributes for
-- the item are deleted. BatchDeleteAttributes is an idempotent operation;
-- running it multiple times on the same item or attribute doesn't result in
-- an error. The BatchDeleteAttributes operation succeeds or fails in its
-- entirety. There are no partial deletes. You can execute multiple
-- BatchDeleteAttributes operations and other operations in parallel. However,
-- large numbers of concurrent BatchDeleteAttributes calls can result in
-- Service Unavailable (503) responses. This operation is vulnerable to
-- exceeding the maximum URL size when making a REST request using the HTTP
-- GET method. This operation does not support conditions using
-- Expected.X.Name, Expected.X.Value, or Expected.X.Exists. The following
-- limitations are enforced for this operation: 1 MB request size 25 item
-- limit per BatchDeleteAttributes operation.
--
-- See: 'Network.AWS.SimpleDB'

batchDeleteAttributes :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => Text -- ^ 'bdaDomainName'
    -> [DeletableItem] -- ^ 'bdaItems'
    -> State BatchDeleteAttributes a
    -> m BatchDeleteAttributesResponse
batchDeleteAttributes p1 p2 s =
    send $ (mkBatchDeleteAttributes p1 p2) &~ s

batchDeleteAttributesCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => Text -- ^ 'bdaDomainName'
    -> [DeletableItem] -- ^ 'bdaItems'
    -> State BatchDeleteAttributes a
    -> m (Either ServiceEr BatchDeleteAttributesResponse)
batchDeleteAttributesCatch p1 p2 s =
    sendCatch $ (mkBatchDeleteAttributes p1 p2) &~ s

-- $BatchPutAttributes
-- The BatchPutAttributes operation creates or replaces attributes within one
-- or more items. By using this operation, the client can perform multiple
-- PutAttribute operation with a single call. This helps yield savings in
-- round trips and latencies, enabling Amazon SimpleDB to optimize requests
-- and generally produce better throughput. The client may specify the item
-- name with the Item.X.ItemName parameter. The client may specify new
-- attributes using a combination of the Item.X.Attribute.Y.Name and
-- Item.X.Attribute.Y.Value parameters. The client may specify the first
-- attribute for the first item using the parameters Item.0.Attribute.0.Name
-- and Item.0.Attribute.0.Value, and for the second attribute for the first
-- item by the parameters Item.0.Attribute.1.Name and
-- Item.0.Attribute.1.Value, and so on. Attributes are uniquely identified
-- within an item by their name/value combination. For example, a single item
-- can have the attributes { "first_name", "first_value" } and { "first_name",
-- "second_value" }. However, it cannot have two attribute instances where
-- both the Item.X.Attribute.Y.Name and Item.X.Attribute.Y.Value are the same.
-- Optionally, the requester can supply the Replace parameter for each
-- individual value. Setting this value to true will cause the new attribute
-- values to replace the existing attribute values. For example, if an item I
-- has the attributes { 'a', '1' }, { 'b', '2'} and { 'b', '3' } and the
-- requester does a BatchPutAttributes of {'I', 'b', '4' } with the Replace
-- parameter set to true, the final attributes of the item will be { 'a', '1'
-- } and { 'b', '4' }, replacing the previous values of the 'b' attribute with
-- the new value. You cannot specify an empty string as an item or as an
-- attribute name. The BatchPutAttributes operation succeeds or fails in its
-- entirety. There are no partial puts. This operation is vulnerable to
-- exceeding the maximum URL size when making a REST request using the HTTP
-- GET method. This operation does not support conditions using
-- Expected.X.Name, Expected.X.Value, or Expected.X.Exists. You can execute
-- multiple BatchPutAttributes operations and other operations in parallel.
-- However, large numbers of concurrent BatchPutAttributes calls can result in
-- Service Unavailable (503) responses. The following limitations are enforced
-- for this operation: 256 attribute name-value pairs per item 1 MB request
-- size 1 billion attributes per domain 10 GB of total user data storage per
-- domain 25 item limit per BatchPutAttributes operation.
--
-- See: 'Network.AWS.SimpleDB'

batchPutAttributes :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => Text -- ^ 'bpaDomainName'
    -> [ReplaceableItem] -- ^ 'bpaItems'
    -> State BatchPutAttributes a
    -> m BatchPutAttributesResponse
batchPutAttributes p1 p2 s =
    send $ (mkBatchPutAttributes p1 p2) &~ s

batchPutAttributesCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => Text -- ^ 'bpaDomainName'
    -> [ReplaceableItem] -- ^ 'bpaItems'
    -> State BatchPutAttributes a
    -> m (Either ServiceEr BatchPutAttributesResponse)
batchPutAttributesCatch p1 p2 s =
    sendCatch $ (mkBatchPutAttributes p1 p2) &~ s

-- $CreateDomain
-- The CreateDomain operation creates a new domain. The domain name should be
-- unique among the domains associated with the Access Key ID provided in the
-- request. The CreateDomain operation may take 10 or more seconds to
-- complete. CreateDomain is an idempotent operation; running it multiple
-- times using the same domain name will not result in an error response. The
-- client can create up to 100 domains per account. If the client requires
-- additional domains, go to
-- http://aws.amazon.com/contact-us/simpledb-limit-request/.
--
-- See: 'Network.AWS.SimpleDB'

createDomain :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'cdDomainName'
    -> State CreateDomain a
    -> m CreateDomainResponse
createDomain p1 s =
    send $ (mkCreateDomain p1) &~ s

createDomainCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'cdDomainName'
    -> State CreateDomain a
    -> m (Either ServiceEr CreateDomainResponse)
createDomainCatch p1 s =
    sendCatch $ (mkCreateDomain p1) &~ s

-- $DeleteAttributes
-- Deletes one or more attributes associated with an item. If all attributes
-- of the item are deleted, the item is deleted. If DeleteAttributes is called
-- without being passed any attributes or values specified, all the attributes
-- for the item are deleted. DeleteAttributes is an idempotent operation;
-- running it multiple times on the same item or attribute does not result in
-- an error response. Because Amazon SimpleDB makes multiple copies of item
-- data and uses an eventual consistency update model, performing a
-- GetAttributes or Select operation (read) immediately after a
-- DeleteAttributes or PutAttributes operation (write) might not return
-- updated item data.
--
-- See: 'Network.AWS.SimpleDB'

deleteAttributes :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'daDomainName'
    -> Text -- ^ 'daItemName'
    -> State DeleteAttributes a
    -> m DeleteAttributesResponse
deleteAttributes p1 p2 s =
    send $ (mkDeleteAttributes p1 p2) &~ s

deleteAttributesCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'daDomainName'
    -> Text -- ^ 'daItemName'
    -> State DeleteAttributes a
    -> m (Either ServiceEr DeleteAttributesResponse)
deleteAttributesCatch p1 p2 s =
    sendCatch $ (mkDeleteAttributes p1 p2) &~ s

-- $DeleteDomain
-- The DeleteDomain operation deletes a domain. Any items (and their
-- attributes) in the domain are deleted as well. The DeleteDomain operation
-- might take 10 or more seconds to complete. Running DeleteDomain on a domain
-- that does not exist or running the function multiple times using the same
-- domain name will not result in an error response.
--
-- See: 'Network.AWS.SimpleDB'

deleteDomain :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'ddDomainName'
    -> State DeleteDomain a
    -> m DeleteDomainResponse
deleteDomain p1 s =
    send $ (mkDeleteDomain p1) &~ s

deleteDomainCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'ddDomainName'
    -> State DeleteDomain a
    -> m (Either ServiceEr DeleteDomainResponse)
deleteDomainCatch p1 s =
    sendCatch $ (mkDeleteDomain p1) &~ s

-- $DomainMetadata
-- Returns information about the domain, including when the domain was
-- created, the number of items and attributes in the domain, and the size of
-- the attribute names and values.
--
-- See: 'Network.AWS.SimpleDB'

domainMetadata :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'dmDomainName'
    -> State DomainMetadata a
    -> m DomainMetadataResponse
domainMetadata p1 s =
    send $ (mkDomainMetadata p1) &~ s

domainMetadataCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'dmDomainName'
    -> State DomainMetadata a
    -> m (Either ServiceEr DomainMetadataResponse)
domainMetadataCatch p1 s =
    sendCatch $ (mkDomainMetadata p1) &~ s

-- $GetAttributes
-- Returns all of the attributes associated with the specified item.
-- Optionally, the attributes returned can be limited to one or more
-- attributes by specifying an attribute name parameter. If the item does not
-- exist on the replica that was accessed for this operation, an empty set is
-- returned. The system does not return an error as it cannot guarantee the
-- item does not exist on other replicas. If GetAttributes is called without
-- being passed any attribute names, all the attributes for the item are
-- returned.
--
-- See: 'Network.AWS.SimpleDB'

getAttributes :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 )
    => Text -- ^ 'gaDomainName'
    -> Text -- ^ 'gaItemName'
    -> State GetAttributes a
    -> m GetAttributesResponse
getAttributes p1 p2 s =
    send $ (mkGetAttributes p1 p2) &~ s

getAttributesCatch :: ( MonadCatch m
                      , MonadResource m
                      , MonadReader Env m
                      )
    => Text -- ^ 'gaDomainName'
    -> Text -- ^ 'gaItemName'
    -> State GetAttributes a
    -> m (Either ServiceEr GetAttributesResponse)
getAttributesCatch p1 p2 s =
    sendCatch $ (mkGetAttributes p1 p2) &~ s

-- $ListDomains
-- The ListDomains operation lists all domains associated with the Access Key
-- ID. It returns domain names up to the limit set by MaxNumberOfDomains. A
-- NextToken is returned if there are more than MaxNumberOfDomains domains.
-- Calling ListDomains successive times with the NextToken provided by the
-- operation returns up to MaxNumberOfDomains more domain names with each
-- successive operation call.
--
-- See: 'Network.AWS.SimpleDB'

listDomains :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env (ResumableSource m)
               )
    => State ListDomains a
    -> ResumableSource m ListDomainsResponse
listDomains s =
    paginate (mkListDomains &~ s)

listDomainsCatch :: ( MonadCatch m
                    , MonadResource m
                    , MonadReader Env (ResumableSource m)
                    )
    => State ListDomains a
    -> ResumableSource m (Either ServiceEr ListDomainsResponse)
listDomainsCatch s =
    paginateCatch (mkListDomains &~ s)

-- $PutAttributes
-- The PutAttributes operation creates or replaces attributes in an item. The
-- client may specify new attributes using a combination of the
-- Attribute.X.Name and Attribute.X.Value parameters. The client specifies the
-- first attribute by the parameters Attribute.0.Name and Attribute.0.Value,
-- the second attribute by the parameters Attribute.1.Name and
-- Attribute.1.Value, and so on. Attributes are uniquely identified in an item
-- by their name/value combination. For example, a single item can have the
-- attributes { "first_name", "first_value" } and { "first_name",
-- second_value" }. However, it cannot have two attribute instances where both
-- the Attribute.X.Name and Attribute.X.Value are the same. Optionally, the
-- requestor can supply the Replace parameter for each individual attribute.
-- Setting this value to true causes the new attribute value to replace the
-- existing attribute value(s). For example, if an item has the attributes {
-- 'a', '1' }, { 'b', '2'} and { 'b', '3' } and the requestor calls
-- PutAttributes using the attributes { 'b', '4' } with the Replace parameter
-- set to true, the final attributes of the item are changed to { 'a', '1' }
-- and { 'b', '4' }, which replaces the previous values of the 'b' attribute
-- with the new value. Using PutAttributes to replace attribute values that do
-- not exist will not result in an error response. You cannot specify an empty
-- string as an attribute name. Because Amazon SimpleDB makes multiple copies
-- of client data and uses an eventual consistency update model, an immediate
-- GetAttributes or Select operation (read) immediately after a PutAttributes
-- or DeleteAttributes operation (write) might not return the updated data.
-- The following limitations are enforced for this operation: 256 total
-- attribute name-value pairs per item One billion attributes per domain 10 GB
-- of total user data storage per domain.
--
-- See: 'Network.AWS.SimpleDB'

putAttributes :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 )
    => Text -- ^ 'paDomainName'
    -> Text -- ^ 'paItemName'
    -> [ReplaceableAttribute] -- ^ 'paAttributes'
    -> State PutAttributes a
    -> m PutAttributesResponse
putAttributes p1 p2 p3 s =
    send $ (mkPutAttributes p1 p2 p3) &~ s

putAttributesCatch :: ( MonadCatch m
                      , MonadResource m
                      , MonadReader Env m
                      )
    => Text -- ^ 'paDomainName'
    -> Text -- ^ 'paItemName'
    -> [ReplaceableAttribute] -- ^ 'paAttributes'
    -> State PutAttributes a
    -> m (Either ServiceEr PutAttributesResponse)
putAttributesCatch p1 p2 p3 s =
    sendCatch $ (mkPutAttributes p1 p2 p3) &~ s

-- $Select
-- The Select operation returns a set of attributes for ItemNames that match
-- the select expression. Select is similar to the standard SQL SELECT
-- statement. The total size of the response cannot exceed 1 MB in total size.
-- Amazon SimpleDB automatically adjusts the number of items returned per page
-- to enforce this limit. For example, if the client asks to retrieve 2500
-- items, but each individual item is 10 kB in size, the system returns 100
-- items and an appropriate NextToken so the client can access the next page
-- of results. For information on how to construct select expressions, see
-- Using Select to Create Amazon SimpleDB Queries in the Developer Guide.
--
-- See: 'Network.AWS.SimpleDB'

select :: ( MonadCatch m
          , MonadResource m
          , MonadError AWS.Error m
          , MonadReader Env (ResumableSource m)
          )
    => Text -- ^ 'sSelectExpression'
    -> State Select a
    -> ResumableSource m SelectResponse
select p1 s =
    paginate $ (mkSelect p1) &~ s

selectCatch :: ( MonadCatch m
               , MonadResource m
               , MonadReader Env (ResumableSource m)
               )
    => Text -- ^ 'sSelectExpression'
    -> State Select a
    -> ResumableSource m (Either ServiceEr SelectResponse)
selectCatch p1 s =
    paginateCatch $ (mkSelect p1) &~ s
