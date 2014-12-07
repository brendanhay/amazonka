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

-- Module      : Network.AWS.IAM.GetAccountAuthorizationDetails
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

-- | Retrieves information about all IAM users, groups, and roles in your account,
-- including their relationships to one another and their attached policies. Use
-- this API to obtain a snapshot of the configuration of IAM permissions (users,
-- groups, roles, and policies) in your account.
--
-- You can optionally filter the results using the 'Filter' parameter. You can
-- paginate the results using the 'MaxItems' and 'Marker' parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetAccountAuthorizationDetails.html>
module Network.AWS.IAM.GetAccountAuthorizationDetails
    (
    -- * Request
      GetAccountAuthorizationDetails
    -- ** Request constructor
    , getAccountAuthorizationDetails
    -- ** Request lenses
    , gaadFilter
    , gaadMarker
    , gaadMaxItems

    -- * Response
    , GetAccountAuthorizationDetailsResponse
    -- ** Response constructor
    , getAccountAuthorizationDetailsResponse
    -- ** Response lenses
    , gaadrGroupDetailList
    , gaadrIsTruncated
    , gaadrMarker
    , gaadrRoleDetailList
    , gaadrUserDetailList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data GetAccountAuthorizationDetails = GetAccountAuthorizationDetails
    { _gaadFilter   :: List "member" EntityType
    , _gaadMarker   :: Maybe Text
    , _gaadMaxItems :: Maybe Nat
    } deriving (Eq, Show)

-- | 'GetAccountAuthorizationDetails' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gaadFilter' @::@ ['EntityType']
--
-- * 'gaadMarker' @::@ 'Maybe' 'Text'
--
-- * 'gaadMaxItems' @::@ 'Maybe' 'Natural'
--
getAccountAuthorizationDetails :: GetAccountAuthorizationDetails
getAccountAuthorizationDetails = GetAccountAuthorizationDetails
    { _gaadFilter   = mempty
    , _gaadMaxItems = Nothing
    , _gaadMarker   = Nothing
    }

-- | A list of entity types (user, group, or role) for filtering the results.
gaadFilter :: Lens' GetAccountAuthorizationDetails [EntityType]
gaadFilter = lens _gaadFilter (\s a -> s { _gaadFilter = a }) . _List

-- | Use this only when paginating results, and only in a subsequent request after
-- you've received a response where the results are truncated. Set it to the
-- value of the 'Marker' element in the response you just received.
gaadMarker :: Lens' GetAccountAuthorizationDetails (Maybe Text)
gaadMarker = lens _gaadMarker (\s a -> s { _gaadMarker = a })

-- | Use this only when paginating results to indicate the maximum number of items
-- you want in the response. If there are additional items beyond the maximum
-- you specify, the 'IsTruncated' response element is 'true'. This parameter is
-- optional. If you do not include it, it defaults to 100.
gaadMaxItems :: Lens' GetAccountAuthorizationDetails (Maybe Natural)
gaadMaxItems = lens _gaadMaxItems (\s a -> s { _gaadMaxItems = a }) . mapping _Nat

data GetAccountAuthorizationDetailsResponse = GetAccountAuthorizationDetailsResponse
    { _gaadrGroupDetailList :: List "member" GroupDetail
    , _gaadrIsTruncated     :: Maybe Bool
    , _gaadrMarker          :: Maybe Text
    , _gaadrRoleDetailList  :: List "member" RoleDetail
    , _gaadrUserDetailList  :: List "member" UserDetail
    } deriving (Eq, Show)

-- | 'GetAccountAuthorizationDetailsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gaadrGroupDetailList' @::@ ['GroupDetail']
--
-- * 'gaadrIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'gaadrMarker' @::@ 'Maybe' 'Text'
--
-- * 'gaadrRoleDetailList' @::@ ['RoleDetail']
--
-- * 'gaadrUserDetailList' @::@ ['UserDetail']
--
getAccountAuthorizationDetailsResponse :: GetAccountAuthorizationDetailsResponse
getAccountAuthorizationDetailsResponse = GetAccountAuthorizationDetailsResponse
    { _gaadrUserDetailList  = mempty
    , _gaadrGroupDetailList = mempty
    , _gaadrRoleDetailList  = mempty
    , _gaadrIsTruncated     = Nothing
    , _gaadrMarker          = Nothing
    }

-- | A list containing information about IAM groups.
gaadrGroupDetailList :: Lens' GetAccountAuthorizationDetailsResponse [GroupDetail]
gaadrGroupDetailList =
    lens _gaadrGroupDetailList (\s a -> s { _gaadrGroupDetailList = a })
        . _List

-- | A flag that indicates whether there are more items to return. If your results
-- were truncated, you can make a subsequent pagination request using the 'Marker'
-- request parameter to retrieve more items.
gaadrIsTruncated :: Lens' GetAccountAuthorizationDetailsResponse (Maybe Bool)
gaadrIsTruncated = lens _gaadrIsTruncated (\s a -> s { _gaadrIsTruncated = a })

-- | If 'IsTruncated' is 'true', this element is present and contains the value to use
-- for the 'Marker' parameter in a subsequent pagination request.
gaadrMarker :: Lens' GetAccountAuthorizationDetailsResponse (Maybe Text)
gaadrMarker = lens _gaadrMarker (\s a -> s { _gaadrMarker = a })

-- | A list containing information about IAM roles.
gaadrRoleDetailList :: Lens' GetAccountAuthorizationDetailsResponse [RoleDetail]
gaadrRoleDetailList =
    lens _gaadrRoleDetailList (\s a -> s { _gaadrRoleDetailList = a })
        . _List

-- | A list containing information about IAM users.
gaadrUserDetailList :: Lens' GetAccountAuthorizationDetailsResponse [UserDetail]
gaadrUserDetailList =
    lens _gaadrUserDetailList (\s a -> s { _gaadrUserDetailList = a })
        . _List

instance ToPath GetAccountAuthorizationDetails where
    toPath = const "/"

instance ToQuery GetAccountAuthorizationDetails where
    toQuery GetAccountAuthorizationDetails{..} = mconcat
        [ "Filter"   =? _gaadFilter
        , "Marker"   =? _gaadMarker
        , "MaxItems" =? _gaadMaxItems
        ]

instance ToHeaders GetAccountAuthorizationDetails

instance AWSRequest GetAccountAuthorizationDetails where
    type Sv GetAccountAuthorizationDetails = IAM
    type Rs GetAccountAuthorizationDetails = GetAccountAuthorizationDetailsResponse

    request  = post "GetAccountAuthorizationDetails"
    response = xmlResponse

instance FromXML GetAccountAuthorizationDetailsResponse where
    parseXML = withElement "GetAccountAuthorizationDetailsResult" $ \x -> GetAccountAuthorizationDetailsResponse
        <$> x .@? "GroupDetailList" .!@ mempty
        <*> x .@? "IsTruncated"
        <*> x .@? "Marker"
        <*> x .@? "RoleDetailList" .!@ mempty
        <*> x .@? "UserDetailList" .!@ mempty
