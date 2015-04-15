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

-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaceBundles
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

-- | Obtains information about the WorkSpace bundles that are available to your
-- account in the specified region.
--
-- You can filter the results with either the 'BundleIds' parameter, or the 'Owner'
-- parameter, but not both.
--
-- This operation supports pagination with the use of the 'NextToken' request and
-- response parameters. If more results are available, the 'NextToken' response
-- member contains a token that you pass in the next call to this operation to
-- retrieve the next set of items.
--
-- <http://docs.aws.amazon.com/workspaces/latest/devguide/API_DescribeWorkspaceBundles.html>
module Network.AWS.WorkSpaces.DescribeWorkspaceBundles
    (
    -- * Request
      DescribeWorkspaceBundles
    -- ** Request constructor
    , describeWorkspaceBundles
    -- ** Request lenses
    , dwbBundleIds
    , dwbNextToken
    , dwbOwner

    -- * Response
    , DescribeWorkspaceBundlesResponse
    -- ** Response constructor
    , describeWorkspaceBundlesResponse
    -- ** Response lenses
    , dwbrBundles
    , dwbrNextToken
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.WorkSpaces.Types
import qualified GHC.Exts

data DescribeWorkspaceBundles = DescribeWorkspaceBundles
    { _dwbBundleIds :: List1 "BundleIds" Text
    , _dwbNextToken :: Maybe Text
    , _dwbOwner     :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeWorkspaceBundles' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwbBundleIds' @::@ 'NonEmpty' 'Text'
--
-- * 'dwbNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dwbOwner' @::@ 'Maybe' 'Text'
--
describeWorkspaceBundles :: NonEmpty Text -- ^ 'dwbBundleIds'
                         -> DescribeWorkspaceBundles
describeWorkspaceBundles p1 = DescribeWorkspaceBundles
    { _dwbBundleIds = withIso _List1 (const id) p1
    , _dwbOwner     = Nothing
    , _dwbNextToken = Nothing
    }

-- | An array of strings that contains the identifiers of the bundles to retrieve.
-- This parameter cannot be combined with any other filter parameter.
dwbBundleIds :: Lens' DescribeWorkspaceBundles (NonEmpty Text)
dwbBundleIds = lens _dwbBundleIds (\s a -> s { _dwbBundleIds = a }) . _List1

-- | The 'NextToken' value from a previous call to this operation. Pass null if this
-- is the first call.
dwbNextToken :: Lens' DescribeWorkspaceBundles (Maybe Text)
dwbNextToken = lens _dwbNextToken (\s a -> s { _dwbNextToken = a })

-- | The owner of the bundles to retrieve. This parameter cannot be combined with
-- any other filter parameter.
--
-- This contains one of the following values:
--
-- null - Retrieves the bundles that belong to the account making the call.  'AMAZON' - Retrieves the bundles that are provided by AWS.
dwbOwner :: Lens' DescribeWorkspaceBundles (Maybe Text)
dwbOwner = lens _dwbOwner (\s a -> s { _dwbOwner = a })

data DescribeWorkspaceBundlesResponse = DescribeWorkspaceBundlesResponse
    { _dwbrBundles   :: List "Bundles" WorkspaceBundle
    , _dwbrNextToken :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'DescribeWorkspaceBundlesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwbrBundles' @::@ ['WorkspaceBundle']
--
-- * 'dwbrNextToken' @::@ 'Maybe' 'Text'
--
describeWorkspaceBundlesResponse :: DescribeWorkspaceBundlesResponse
describeWorkspaceBundlesResponse = DescribeWorkspaceBundlesResponse
    { _dwbrBundles   = mempty
    , _dwbrNextToken = Nothing
    }

-- | An array of structures that contain information about the bundles.
dwbrBundles :: Lens' DescribeWorkspaceBundlesResponse [WorkspaceBundle]
dwbrBundles = lens _dwbrBundles (\s a -> s { _dwbrBundles = a }) . _List

-- | If not null, more results are available. Pass this value for the 'NextToken'
-- parameter in a subsequent call to this operation to retrieve the next set of
-- items. This token is valid for one day and must be used within that timeframe.
dwbrNextToken :: Lens' DescribeWorkspaceBundlesResponse (Maybe Text)
dwbrNextToken = lens _dwbrNextToken (\s a -> s { _dwbrNextToken = a })

instance ToPath DescribeWorkspaceBundles where
    toPath = const "/"

instance ToQuery DescribeWorkspaceBundles where
    toQuery = const mempty

instance ToHeaders DescribeWorkspaceBundles

instance ToJSON DescribeWorkspaceBundles where
    toJSON DescribeWorkspaceBundles{..} = object
        [ "BundleIds" .= _dwbBundleIds
        , "Owner"     .= _dwbOwner
        , "NextToken" .= _dwbNextToken
        ]

instance AWSRequest DescribeWorkspaceBundles where
    type Sv DescribeWorkspaceBundles = WorkSpaces
    type Rs DescribeWorkspaceBundles = DescribeWorkspaceBundlesResponse

    request  = post "DescribeWorkspaceBundles"
    response = jsonResponse

instance FromJSON DescribeWorkspaceBundlesResponse where
    parseJSON = withObject "DescribeWorkspaceBundlesResponse" $ \o -> DescribeWorkspaceBundlesResponse
        <$> o .:? "Bundles" .!= mempty
        <*> o .:? "NextToken"
