{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.DeleteIndexField
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes an IndexField from the search domain. For more information, see
-- Configuring Index Fields in the Amazon CloudSearch Developer Guide.
module Network.AWS.CloudSearch.DeleteIndexField
    (
    -- * Request
      DeleteIndexField
    -- ** Request constructor
    , deleteIndexField
    -- ** Request lenses
    , dif1DomainName
    , dif1IndexFieldName

    -- * Response
    , DeleteIndexFieldResponse
    -- ** Response constructor
    , deleteIndexFieldResponse
    -- ** Response lenses
    , difrrIndexField
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import Network.AWS.Prelude

-- | Container for the parameters to the DeleteIndexField operation. Specifies
-- the name of the domain you want to update and the name of the index field
-- you want to delete.
data DeleteIndexField = DeleteIndexField
    { _dif1DomainName :: Text
    , _dif1IndexFieldName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteIndexField' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainName ::@ @Text@
--
-- * @IndexFieldName ::@ @Text@
--
deleteIndexField :: Text -- ^ 'dif1DomainName'
                 -> Text -- ^ 'dif1IndexFieldName'
                 -> DeleteIndexField
deleteIndexField p1 p2 = DeleteIndexField
    { _dif1DomainName = p1
    , _dif1IndexFieldName = p2
    }

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
dif1DomainName :: Lens' DeleteIndexField Text
dif1DomainName = lens _dif1DomainName (\s a -> s { _dif1DomainName = a })

-- | The name of the index field your want to remove from the domain's indexing
-- options.
dif1IndexFieldName :: Lens' DeleteIndexField Text
dif1IndexFieldName =
    lens _dif1IndexFieldName (\s a -> s { _dif1IndexFieldName = a })

instance ToQuery DeleteIndexField where
    toQuery = genericQuery def

-- | The result of a DeleteIndexField request.
newtype DeleteIndexFieldResponse = DeleteIndexFieldResponse
    { _difrrIndexField :: IndexFieldStatus
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteIndexFieldResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IndexField ::@ @IndexFieldStatus@
--
deleteIndexFieldResponse :: IndexFieldStatus -- ^ 'difrrIndexField'
                         -> DeleteIndexFieldResponse
deleteIndexFieldResponse p1 = DeleteIndexFieldResponse
    { _difrrIndexField = p1
    }

-- | The status of the index field being deleted.
difrrIndexField :: Lens' DeleteIndexFieldResponse IndexFieldStatus
difrrIndexField = lens _difrrIndexField (\s a -> s { _difrrIndexField = a })

instance FromXML DeleteIndexFieldResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteIndexField where
    type Sv DeleteIndexField = CloudSearch
    type Rs DeleteIndexField = DeleteIndexFieldResponse

    request = post "DeleteIndexField"
    response _ = xmlResponse
