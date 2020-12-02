{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Permission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Permission where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The @Permission@ structure.
--
--
--
-- /See:/ 'permission' smart constructor.
data Permission = Permission'
  { _pAccess :: !(Maybe [Text]),
    _pGranteeType :: !(Maybe Text),
    _pGrantee :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Permission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pAccess' - The permission that you want to give to the AWS user that is listed in Grantee. Valid values include:      * @READ@ : The grantee can read the thumbnails and metadata for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @READ_ACP@ : The grantee can read the object ACL for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @WRITE_ACP@ : The grantee can write the ACL for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @FULL_CONTROL@ : The grantee has READ, READ_ACP, and WRITE_ACP permissions for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
--
-- * 'pGranteeType' - The type of value that appears in the Grantee object:     * @Canonical@ : Either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution. /Important:/ A canonical user ID is not the same as an AWS account number.     * @Email@ : The registered email address of an AWS account.     * @Group@ : One of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .
--
-- * 'pGrantee' - The AWS user or group that you want to have access to transcoded files and playlists. To identify the user or group, you can specify the canonical user ID for an AWS account, an origin access identity for a CloudFront distribution, the registered email address of an AWS account, or a predefined Amazon S3 group.
permission ::
  Permission
permission =
  Permission'
    { _pAccess = Nothing,
      _pGranteeType = Nothing,
      _pGrantee = Nothing
    }

-- | The permission that you want to give to the AWS user that is listed in Grantee. Valid values include:      * @READ@ : The grantee can read the thumbnails and metadata for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @READ_ACP@ : The grantee can read the object ACL for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @WRITE_ACP@ : The grantee can write the ACL for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @FULL_CONTROL@ : The grantee has READ, READ_ACP, and WRITE_ACP permissions for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
pAccess :: Lens' Permission [Text]
pAccess = lens _pAccess (\s a -> s {_pAccess = a}) . _Default . _Coerce

-- | The type of value that appears in the Grantee object:     * @Canonical@ : Either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution. /Important:/ A canonical user ID is not the same as an AWS account number.     * @Email@ : The registered email address of an AWS account.     * @Group@ : One of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .
pGranteeType :: Lens' Permission (Maybe Text)
pGranteeType = lens _pGranteeType (\s a -> s {_pGranteeType = a})

-- | The AWS user or group that you want to have access to transcoded files and playlists. To identify the user or group, you can specify the canonical user ID for an AWS account, an origin access identity for a CloudFront distribution, the registered email address of an AWS account, or a predefined Amazon S3 group.
pGrantee :: Lens' Permission (Maybe Text)
pGrantee = lens _pGrantee (\s a -> s {_pGrantee = a})

instance FromJSON Permission where
  parseJSON =
    withObject
      "Permission"
      ( \x ->
          Permission'
            <$> (x .:? "Access" .!= mempty)
            <*> (x .:? "GranteeType")
            <*> (x .:? "Grantee")
      )

instance Hashable Permission

instance NFData Permission

instance ToJSON Permission where
  toJSON Permission' {..} =
    object
      ( catMaybes
          [ ("Access" .=) <$> _pAccess,
            ("GranteeType" .=) <$> _pGranteeType,
            ("Grantee" .=) <$> _pGrantee
          ]
      )
