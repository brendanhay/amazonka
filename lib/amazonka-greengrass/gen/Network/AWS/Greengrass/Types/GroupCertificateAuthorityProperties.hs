{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.GroupCertificateAuthorityProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.GroupCertificateAuthorityProperties where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a certificate authority for a group.
--
-- /See:/ 'groupCertificateAuthorityProperties' smart constructor.
data GroupCertificateAuthorityProperties = GroupCertificateAuthorityProperties'
  { _gcapGroupCertificateAuthorityARN ::
      !(Maybe Text),
    _gcapGroupCertificateAuthorityId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GroupCertificateAuthorityProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcapGroupCertificateAuthorityARN' - The ARN of the certificate authority for the group.
--
-- * 'gcapGroupCertificateAuthorityId' - The ID of the certificate authority for the group.
groupCertificateAuthorityProperties ::
  GroupCertificateAuthorityProperties
groupCertificateAuthorityProperties =
  GroupCertificateAuthorityProperties'
    { _gcapGroupCertificateAuthorityARN =
        Nothing,
      _gcapGroupCertificateAuthorityId = Nothing
    }

-- | The ARN of the certificate authority for the group.
gcapGroupCertificateAuthorityARN :: Lens' GroupCertificateAuthorityProperties (Maybe Text)
gcapGroupCertificateAuthorityARN = lens _gcapGroupCertificateAuthorityARN (\s a -> s {_gcapGroupCertificateAuthorityARN = a})

-- | The ID of the certificate authority for the group.
gcapGroupCertificateAuthorityId :: Lens' GroupCertificateAuthorityProperties (Maybe Text)
gcapGroupCertificateAuthorityId = lens _gcapGroupCertificateAuthorityId (\s a -> s {_gcapGroupCertificateAuthorityId = a})

instance FromJSON GroupCertificateAuthorityProperties where
  parseJSON =
    withObject
      "GroupCertificateAuthorityProperties"
      ( \x ->
          GroupCertificateAuthorityProperties'
            <$> (x .:? "GroupCertificateAuthorityArn")
            <*> (x .:? "GroupCertificateAuthorityId")
      )

instance Hashable GroupCertificateAuthorityProperties

instance NFData GroupCertificateAuthorityProperties
