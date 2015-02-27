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

-- Module      : Network.AWS.IAM.ListPolicyVersions
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

-- | Lists information about the versions of the specified managed policy,
-- including the version that is set as the policy's default version.
--
-- For more information about managed policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies andInline Policies> in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListPolicyVersions.html>
module Network.AWS.IAM.ListPolicyVersions
    (
    -- * Request
      ListPolicyVersions
    -- ** Request constructor
    , listPolicyVersions
    -- ** Request lenses
    , lpvMarker
    , lpvMaxItems
    , lpvPolicyArn

    -- * Response
    , ListPolicyVersionsResponse
    -- ** Response constructor
    , listPolicyVersionsResponse
    -- ** Response lenses
    , lpvrIsTruncated
    , lpvrMarker
    , lpvrVersions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data ListPolicyVersions = ListPolicyVersions
    { _lpvMarker    :: Maybe Text
    , _lpvMaxItems  :: Maybe Nat
    , _lpvPolicyArn :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListPolicyVersions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpvMarker' @::@ 'Maybe' 'Text'
--
-- * 'lpvMaxItems' @::@ 'Maybe' 'Natural'
--
-- * 'lpvPolicyArn' @::@ 'Text'
--
listPolicyVersions :: Text -- ^ 'lpvPolicyArn'
                   -> ListPolicyVersions
listPolicyVersions p1 = ListPolicyVersions
    { _lpvPolicyArn = p1
    , _lpvMarker    = Nothing
    , _lpvMaxItems  = Nothing
    }

-- | Use this parameter only when paginating results, and only in a subsequent
-- request after you've received a response where the results are truncated. Set
-- it to the value of the 'Marker' element in the response you just received.
lpvMarker :: Lens' ListPolicyVersions (Maybe Text)
lpvMarker = lens _lpvMarker (\s a -> s { _lpvMarker = a })

-- | Use this parameter only when paginating results to indicate the maximum
-- number of policy versions you want in the response. If there are additional
-- policy versions beyond the maximum you specify, the 'IsTruncated' response
-- element is 'true'. This parameter is optional. If you do not include it, it
-- defaults to 100.
lpvMaxItems :: Lens' ListPolicyVersions (Maybe Natural)
lpvMaxItems = lens _lpvMaxItems (\s a -> s { _lpvMaxItems = a }) . mapping _Nat

lpvPolicyArn :: Lens' ListPolicyVersions Text
lpvPolicyArn = lens _lpvPolicyArn (\s a -> s { _lpvPolicyArn = a })

data ListPolicyVersionsResponse = ListPolicyVersionsResponse
    { _lpvrIsTruncated :: Maybe Bool
    , _lpvrMarker      :: Maybe Text
    , _lpvrVersions    :: List "member" PolicyVersion
    } deriving (Eq, Read, Show)

-- | 'ListPolicyVersionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpvrIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'lpvrMarker' @::@ 'Maybe' 'Text'
--
-- * 'lpvrVersions' @::@ ['PolicyVersion']
--
listPolicyVersionsResponse :: ListPolicyVersionsResponse
listPolicyVersionsResponse = ListPolicyVersionsResponse
    { _lpvrVersions    = mempty
    , _lpvrIsTruncated = Nothing
    , _lpvrMarker      = Nothing
    }

-- | A flag that indicates whether there are more policy versions to list. If your
-- results were truncated, you can make a subsequent pagination request using
-- the 'Marker' request parameter to retrieve more policy versions in the list.
lpvrIsTruncated :: Lens' ListPolicyVersionsResponse (Maybe Bool)
lpvrIsTruncated = lens _lpvrIsTruncated (\s a -> s { _lpvrIsTruncated = a })

-- | If 'IsTruncated' is 'true', this element is present and contains the value to use
-- for the 'Marker' parameter in a subsequent pagination request.
lpvrMarker :: Lens' ListPolicyVersionsResponse (Maybe Text)
lpvrMarker = lens _lpvrMarker (\s a -> s { _lpvrMarker = a })

-- | A list of policy versions.
--
-- For more information about managed policy versions, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning forManaged Policies> in the /Using IAM/ guide.
lpvrVersions :: Lens' ListPolicyVersionsResponse [PolicyVersion]
lpvrVersions = lens _lpvrVersions (\s a -> s { _lpvrVersions = a }) . _List

instance ToPath ListPolicyVersions where
    toPath = const "/"

instance ToQuery ListPolicyVersions where
    toQuery ListPolicyVersions{..} = mconcat
        [ "Marker"    =? _lpvMarker
        , "MaxItems"  =? _lpvMaxItems
        , "PolicyArn" =? _lpvPolicyArn
        ]

instance ToHeaders ListPolicyVersions

instance AWSRequest ListPolicyVersions where
    type Sv ListPolicyVersions = IAM
    type Rs ListPolicyVersions = ListPolicyVersionsResponse

    request  = post "ListPolicyVersions"
    response = xmlResponse

instance FromXML ListPolicyVersionsResponse where
    parseXML = withElement "ListPolicyVersionsResult" $ \x -> ListPolicyVersionsResponse
        <$> x .@? "IsTruncated"
        <*> x .@? "Marker"
        <*> x .@? "Versions" .!@ mempty
