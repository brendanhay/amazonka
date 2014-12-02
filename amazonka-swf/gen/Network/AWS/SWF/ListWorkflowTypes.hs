{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.ListWorkflowTypes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns information about workflow types in the specified domain. The
-- results may be split into multiple pages that can be retrieved by making the
-- call repeatedly.
--
-- Access Control
--
-- You can use IAM policies to control this action's access to Amazon SWF
-- resources as follows:
--
-- Use a 'Resource' element with the domain name to limit the action to only
-- specified domains. Use an 'Action' element to allow or deny permission to call
-- this action. You cannot use an IAM policy to constrain this action's
-- parameters.  If the caller does not have sufficient permissions to invoke the
-- action, or the parameter values fall outside the specified constraints, the
-- action fails by throwing 'OperationNotPermitted'. For details and example IAM
-- policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_ListWorkflowTypes.html>
module Network.AWS.SWF.ListWorkflowTypes
    (
    -- * Request
      ListWorkflowTypes
    -- ** Request constructor
    , listWorkflowTypes
    -- ** Request lenses
    , lwtDomain
    , lwtMaximumPageSize
    , lwtName
    , lwtNextPageToken
    , lwtRegistrationStatus
    , lwtReverseOrder

    -- * Response
    , ListWorkflowTypesResponse
    -- ** Response constructor
    , listWorkflowTypesResponse
    -- ** Response lenses
    , lwtrNextPageToken
    , lwtrTypeInfos
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SWF.Types
import qualified GHC.Exts

data ListWorkflowTypes = ListWorkflowTypes
    { _lwtDomain             :: Text
    , _lwtMaximumPageSize    :: Maybe Nat
    , _lwtName               :: Maybe Text
    , _lwtNextPageToken      :: Maybe Text
    , _lwtRegistrationStatus :: RegistrationStatus
    , _lwtReverseOrder       :: Maybe Bool
    } deriving (Eq, Show)

-- | 'ListWorkflowTypes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lwtDomain' @::@ 'Text'
--
-- * 'lwtMaximumPageSize' @::@ 'Maybe' 'Natural'
--
-- * 'lwtName' @::@ 'Maybe' 'Text'
--
-- * 'lwtNextPageToken' @::@ 'Maybe' 'Text'
--
-- * 'lwtRegistrationStatus' @::@ 'RegistrationStatus'
--
-- * 'lwtReverseOrder' @::@ 'Maybe' 'Bool'
--
listWorkflowTypes :: Text -- ^ 'lwtDomain'
                  -> RegistrationStatus -- ^ 'lwtRegistrationStatus'
                  -> ListWorkflowTypes
listWorkflowTypes p1 p2 = ListWorkflowTypes
    { _lwtDomain             = p1
    , _lwtRegistrationStatus = p2
    , _lwtName               = Nothing
    , _lwtNextPageToken      = Nothing
    , _lwtMaximumPageSize    = Nothing
    , _lwtReverseOrder       = Nothing
    }

-- | The name of the domain in which the workflow types have been registered.
lwtDomain :: Lens' ListWorkflowTypes Text
lwtDomain = lens _lwtDomain (\s a -> s { _lwtDomain = a })

-- | The maximum number of results returned in each page. The default is 100, but
-- the caller can override this value to a page size /smaller/ than the default.
-- You cannot specify a page size greater than 100. Note that the number of
-- types may be less than the maxiumum page size, in which case, the returned
-- page will have fewer results than the maximumPageSize specified.
lwtMaximumPageSize :: Lens' ListWorkflowTypes (Maybe Natural)
lwtMaximumPageSize =
    lens _lwtMaximumPageSize (\s a -> s { _lwtMaximumPageSize = a })
        . mapping _Nat

-- | If specified, lists the workflow type with this name.
lwtName :: Lens' ListWorkflowTypes (Maybe Text)
lwtName = lens _lwtName (\s a -> s { _lwtName = a })

-- | If on a previous call to this method a 'NextPageToken' was returned, the
-- results are being paginated. To get the next page of results, repeat the call
-- with the returned token and all other arguments unchanged.
lwtNextPageToken :: Lens' ListWorkflowTypes (Maybe Text)
lwtNextPageToken = lens _lwtNextPageToken (\s a -> s { _lwtNextPageToken = a })

-- | Specifies the registration status of the workflow types to list.
lwtRegistrationStatus :: Lens' ListWorkflowTypes RegistrationStatus
lwtRegistrationStatus =
    lens _lwtRegistrationStatus (\s a -> s { _lwtRegistrationStatus = a })

-- | When set to 'true', returns the results in reverse order. By default the
-- results are returned in ascending alphabetical order of the 'name' of the
-- workflow types.
lwtReverseOrder :: Lens' ListWorkflowTypes (Maybe Bool)
lwtReverseOrder = lens _lwtReverseOrder (\s a -> s { _lwtReverseOrder = a })

data ListWorkflowTypesResponse = ListWorkflowTypesResponse
    { _lwtrNextPageToken :: Maybe Text
    , _lwtrTypeInfos     :: List "typeInfos" WorkflowTypeInfo
    } deriving (Eq, Show)

-- | 'ListWorkflowTypesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lwtrNextPageToken' @::@ 'Maybe' 'Text'
--
-- * 'lwtrTypeInfos' @::@ ['WorkflowTypeInfo']
--
listWorkflowTypesResponse :: ListWorkflowTypesResponse
listWorkflowTypesResponse = ListWorkflowTypesResponse
    { _lwtrTypeInfos     = mempty
    , _lwtrNextPageToken = Nothing
    }

-- | The token for the next page of type information. If set then the list
-- consists of more than one page. You can retrieve the next page by repeating
-- the request (that returned the structure) with the this token and all other
-- arguments unchanged.
lwtrNextPageToken :: Lens' ListWorkflowTypesResponse (Maybe Text)
lwtrNextPageToken =
    lens _lwtrNextPageToken (\s a -> s { _lwtrNextPageToken = a })

-- | The list of workflow type information.
lwtrTypeInfos :: Lens' ListWorkflowTypesResponse [WorkflowTypeInfo]
lwtrTypeInfos = lens _lwtrTypeInfos (\s a -> s { _lwtrTypeInfos = a }) . _List

instance ToPath ListWorkflowTypes where
    toPath = const "/"

instance ToQuery ListWorkflowTypes where
    toQuery = const mempty

instance ToHeaders ListWorkflowTypes

instance ToJSON ListWorkflowTypes where
    toJSON ListWorkflowTypes{..} = object
        [ "domain"             .= _lwtDomain
        , "name"               .= _lwtName
        , "registrationStatus" .= _lwtRegistrationStatus
        , "nextPageToken"      .= _lwtNextPageToken
        , "maximumPageSize"    .= _lwtMaximumPageSize
        , "reverseOrder"       .= _lwtReverseOrder
        ]

instance AWSRequest ListWorkflowTypes where
    type Sv ListWorkflowTypes = SWF
    type Rs ListWorkflowTypes = ListWorkflowTypesResponse

    request  = post "ListWorkflowTypes"
    response = jsonResponse

instance FromJSON ListWorkflowTypesResponse where
    parseJSON = withObject "ListWorkflowTypesResponse" $ \o -> ListWorkflowTypesResponse
        <$> o .:? "nextPageToken"
        <*> o .:? "typeInfos" .!= mempty

instance AWSPager ListWorkflowTypes where
    page rq rs
        | stop (rq ^. lwtNextPageToken) = Nothing
        | otherwise = (\x -> rq & lwtNextPageToken ?~ x)
            <$> (rs ^. lwtrNextPageToken)
