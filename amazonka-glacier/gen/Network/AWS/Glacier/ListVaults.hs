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

-- Module      : Network.AWS.Glacier.ListVaults
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

-- | This operation lists all vaults owned by the calling user's account. The list
-- returned in the response is ASCII-sorted by vault name.
--
-- By default, this operation returns up to 1,000 items. If there are more
-- vaults to list, the response 'marker' field contains the vault Amazon Resource
-- Name (ARN) at which to continue the list with a new List Vaults request;
-- otherwise, the 'marker' field is 'null'. To return a list of vaults that begins
-- at a specific vault, set the 'marker' request parameter to the vault ARN you
-- obtained from a previous List Vaults request. You can also limit the number
-- of vaults returned in the response by specifying the 'limit' parameter in the
-- request.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don't have any
-- permissions by default. You must grant them explicit permission to perform
-- specific actions. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identityand Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, go to <http://docs.aws.amazon.com/amazonglacier/latest/dev/retrieving-vault-info.html Retrieving VaultMetadata in Amazon Glacier> and <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vaults-get.html List Vaults > in the /Amazon Glacier DeveloperGuide/.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-ListVaults.html>
module Network.AWS.Glacier.ListVaults
    (
    -- * Request
      ListVaults
    -- ** Request constructor
    , listVaults
    -- ** Request lenses
    , lvAccountId
    , lvLimit
    , lvMarker

    -- * Response
    , ListVaultsResponse
    -- ** Response constructor
    , listVaultsResponse
    -- ** Response lenses
    , lvrMarker
    , lvrVaultList
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Glacier.Types
import qualified GHC.Exts

data ListVaults = ListVaults
    { _lvAccountId :: Text
    , _lvLimit     :: Maybe Text
    , _lvMarker    :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListVaults' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lvAccountId' @::@ 'Text'
--
-- * 'lvLimit' @::@ 'Maybe' 'Text'
--
-- * 'lvMarker' @::@ 'Maybe' 'Text'
--
listVaults :: Text -- ^ 'lvAccountId'
           -> ListVaults
listVaults p1 = ListVaults
    { _lvAccountId = p1
    , _lvMarker    = Nothing
    , _lvLimit     = Nothing
    }

-- | The 'AccountId' is the AWS Account ID. You can specify either the AWS Account
-- ID or optionally a '-', in which case Amazon Glacier uses the AWS Account ID
-- associated with the credentials used to sign the request. If you specify your
-- Account ID, do not include hyphens in it.
lvAccountId :: Lens' ListVaults Text
lvAccountId = lens _lvAccountId (\s a -> s { _lvAccountId = a })

-- | The maximum number of items returned in the response. If you don't specify a
-- value, the List Vaults operation returns up to 1,000 items.
lvLimit :: Lens' ListVaults (Maybe Text)
lvLimit = lens _lvLimit (\s a -> s { _lvLimit = a })

-- | A string used for pagination. The marker specifies the vault ARN after which
-- the listing of vaults should begin.
lvMarker :: Lens' ListVaults (Maybe Text)
lvMarker = lens _lvMarker (\s a -> s { _lvMarker = a })

data ListVaultsResponse = ListVaultsResponse
    { _lvrMarker    :: Maybe Text
    , _lvrVaultList :: List "VaultList" DescribeVaultOutput
    } deriving (Eq, Read, Show)

-- | 'ListVaultsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lvrMarker' @::@ 'Maybe' 'Text'
--
-- * 'lvrVaultList' @::@ ['DescribeVaultOutput']
--
listVaultsResponse :: ListVaultsResponse
listVaultsResponse = ListVaultsResponse
    { _lvrVaultList = mempty
    , _lvrMarker    = Nothing
    }

-- | The vault ARN at which to continue pagination of the results. You use the
-- marker in another List Vaults request to obtain more vaults in the list.
lvrMarker :: Lens' ListVaultsResponse (Maybe Text)
lvrMarker = lens _lvrMarker (\s a -> s { _lvrMarker = a })

-- | List of vaults.
lvrVaultList :: Lens' ListVaultsResponse [DescribeVaultOutput]
lvrVaultList = lens _lvrVaultList (\s a -> s { _lvrVaultList = a }) . _List

instance ToPath ListVaults where
    toPath ListVaults{..} = mconcat
        [ "/"
        , toText _lvAccountId
        , "/vaults"
        ]

instance ToQuery ListVaults where
    toQuery ListVaults{..} = mconcat
        [ "marker" =? _lvMarker
        , "limit"  =? _lvLimit
        ]

instance ToHeaders ListVaults

instance ToJSON ListVaults where
    toJSON = const (toJSON Empty)

instance AWSRequest ListVaults where
    type Sv ListVaults = Glacier
    type Rs ListVaults = ListVaultsResponse

    request  = get
    response = jsonResponse

instance FromJSON ListVaultsResponse where
    parseJSON = withObject "ListVaultsResponse" $ \o -> ListVaultsResponse
        <$> o .:? "Marker"
        <*> o .:? "VaultList" .!= mempty
