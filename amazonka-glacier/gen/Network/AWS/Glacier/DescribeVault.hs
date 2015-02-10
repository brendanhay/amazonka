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

-- Module      : Network.AWS.Glacier.DescribeVault
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

-- | This operation returns information about a vault, including the vault's
-- Amazon Resource Name (ARN), the date the vault was created, the number of
-- archives it contains, and the total size of all the archives in the vault.
-- The number of archives and their total size are as of the last inventory
-- generation. This means that if you add or remove an archive from a vault, and
-- then immediately use Describe Vault, the change in contents will not be
-- immediately reflected. If you want to retrieve the latest inventory of the
-- vault, use 'InitiateJob'. Amazon Glacier generates vault inventories
-- approximately daily. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-inventory.html Downloading a Vault Inventoryin Amazon Glacier>.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don't have any
-- permissions by default. You must grant them explicit permission to perform
-- specific actions. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identityand Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, go to <http://docs.aws.amazon.com/amazonglacier/latest/dev/retrieving-vault-info.html Retrieving VaultMetadata in Amazon Glacier> and <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-get.html Describe Vault > in the /Amazon GlacierDeveloper Guide/.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-DescribeVault.html>
module Network.AWS.Glacier.DescribeVault
    (
    -- * Request
      DescribeVault
    -- ** Request constructor
    , describeVault
    -- ** Request lenses
    , dvAccountId
    , dvVaultName

    -- * Response
    , DescribeVaultResponse
    -- ** Response constructor
    , describeVaultResponse
    -- ** Response lenses
    , dvrCreationDate
    , dvrLastInventoryDate
    , dvrNumberOfArchives
    , dvrSizeInBytes
    , dvrVaultARN
    , dvrVaultName
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Glacier.Types
import qualified GHC.Exts

data DescribeVault = DescribeVault
    { _dvAccountId :: Text
    , _dvVaultName :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeVault' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvAccountId' @::@ 'Text'
--
-- * 'dvVaultName' @::@ 'Text'
--
describeVault :: Text -- ^ 'dvAccountId'
              -> Text -- ^ 'dvVaultName'
              -> DescribeVault
describeVault p1 p2 = DescribeVault
    { _dvAccountId = p1
    , _dvVaultName = p2
    }

-- | The 'AccountId' is the AWS Account ID. You can specify either the AWS Account
-- ID or optionally a '-', in which case Amazon Glacier uses the AWS Account ID
-- associated with the credentials used to sign the request. If you specify your
-- Account ID, do not include hyphens in it.
dvAccountId :: Lens' DescribeVault Text
dvAccountId = lens _dvAccountId (\s a -> s { _dvAccountId = a })

-- | The name of the vault.
dvVaultName :: Lens' DescribeVault Text
dvVaultName = lens _dvVaultName (\s a -> s { _dvVaultName = a })

data DescribeVaultResponse = DescribeVaultResponse
    { _dvrCreationDate      :: Maybe Text
    , _dvrLastInventoryDate :: Maybe Text
    , _dvrNumberOfArchives  :: Maybe Integer
    , _dvrSizeInBytes       :: Maybe Integer
    , _dvrVaultARN          :: Maybe Text
    , _dvrVaultName         :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeVaultResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvrCreationDate' @::@ 'Maybe' 'Text'
--
-- * 'dvrLastInventoryDate' @::@ 'Maybe' 'Text'
--
-- * 'dvrNumberOfArchives' @::@ 'Maybe' 'Integer'
--
-- * 'dvrSizeInBytes' @::@ 'Maybe' 'Integer'
--
-- * 'dvrVaultARN' @::@ 'Maybe' 'Text'
--
-- * 'dvrVaultName' @::@ 'Maybe' 'Text'
--
describeVaultResponse :: DescribeVaultResponse
describeVaultResponse = DescribeVaultResponse
    { _dvrVaultARN          = Nothing
    , _dvrVaultName         = Nothing
    , _dvrCreationDate      = Nothing
    , _dvrLastInventoryDate = Nothing
    , _dvrNumberOfArchives  = Nothing
    , _dvrSizeInBytes       = Nothing
    }

-- | The UTC date when the vault was created. A string representation of ISO 8601
-- date format, for example, "2012-03-20T17:03:43.221Z".
dvrCreationDate :: Lens' DescribeVaultResponse (Maybe Text)
dvrCreationDate = lens _dvrCreationDate (\s a -> s { _dvrCreationDate = a })

-- | The UTC date when Amazon Glacier completed the last vault inventory. A string
-- representation of ISO 8601 date format, for example,
-- "2012-03-20T17:03:43.221Z".
dvrLastInventoryDate :: Lens' DescribeVaultResponse (Maybe Text)
dvrLastInventoryDate =
    lens _dvrLastInventoryDate (\s a -> s { _dvrLastInventoryDate = a })

-- | The number of archives in the vault as of the last inventory date. This field
-- will return 'null' if an inventory has not yet run on the vault, for example,
-- if you just created the vault.
dvrNumberOfArchives :: Lens' DescribeVaultResponse (Maybe Integer)
dvrNumberOfArchives =
    lens _dvrNumberOfArchives (\s a -> s { _dvrNumberOfArchives = a })

-- | Total size, in bytes, of the archives in the vault as of the last inventory
-- date. This field will return null if an inventory has not yet run on the
-- vault, for example, if you just created the vault.
dvrSizeInBytes :: Lens' DescribeVaultResponse (Maybe Integer)
dvrSizeInBytes = lens _dvrSizeInBytes (\s a -> s { _dvrSizeInBytes = a })

-- | The Amazon Resource Name (ARN) of the vault.
dvrVaultARN :: Lens' DescribeVaultResponse (Maybe Text)
dvrVaultARN = lens _dvrVaultARN (\s a -> s { _dvrVaultARN = a })

-- | The name of the vault.
dvrVaultName :: Lens' DescribeVaultResponse (Maybe Text)
dvrVaultName = lens _dvrVaultName (\s a -> s { _dvrVaultName = a })

instance ToPath DescribeVault where
    toPath DescribeVault{..} = mconcat
        [ "/"
        , toText _dvAccountId
        , "/vaults/"
        , toText _dvVaultName
        ]

instance ToQuery DescribeVault where
    toQuery = const mempty

instance ToHeaders DescribeVault

instance ToJSON DescribeVault where
    toJSON = const (toJSON Empty)

instance AWSRequest DescribeVault where
    type Sv DescribeVault = Glacier
    type Rs DescribeVault = DescribeVaultResponse

    request  = get
    response = jsonResponse

instance FromJSON DescribeVaultResponse where
    parseJSON = withObject "DescribeVaultResponse" $ \o -> DescribeVaultResponse
        <$> o .:? "CreationDate"
        <*> o .:? "LastInventoryDate"
        <*> o .:? "NumberOfArchives"
        <*> o .:? "SizeInBytes"
        <*> o .:? "VaultARN"
        <*> o .:? "VaultName"
