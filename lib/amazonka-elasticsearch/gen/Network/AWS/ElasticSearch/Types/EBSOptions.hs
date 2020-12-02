{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.EBSOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.EBSOptions where

import Network.AWS.ElasticSearch.Types.VolumeType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Options to enable, disable, and specify the properties of EBS storage volumes. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs Configuring EBS-based Storage> .
--
--
--
-- /See:/ 'ebsOptions' smart constructor.
data EBSOptions = EBSOptions'
  { _eoVolumeSize :: !(Maybe Int),
    _eoIOPS :: !(Maybe Int),
    _eoVolumeType :: !(Maybe VolumeType),
    _eoEBSEnabled :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EBSOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eoVolumeSize' - Integer to specify the size of an EBS volume.
--
-- * 'eoIOPS' - Specifies the IOPD for a Provisioned IOPS EBS volume (SSD).
--
-- * 'eoVolumeType' - Specifies the volume type for EBS-based storage.
--
-- * 'eoEBSEnabled' - Specifies whether EBS-based storage is enabled.
ebsOptions ::
  EBSOptions
ebsOptions =
  EBSOptions'
    { _eoVolumeSize = Nothing,
      _eoIOPS = Nothing,
      _eoVolumeType = Nothing,
      _eoEBSEnabled = Nothing
    }

-- | Integer to specify the size of an EBS volume.
eoVolumeSize :: Lens' EBSOptions (Maybe Int)
eoVolumeSize = lens _eoVolumeSize (\s a -> s {_eoVolumeSize = a})

-- | Specifies the IOPD for a Provisioned IOPS EBS volume (SSD).
eoIOPS :: Lens' EBSOptions (Maybe Int)
eoIOPS = lens _eoIOPS (\s a -> s {_eoIOPS = a})

-- | Specifies the volume type for EBS-based storage.
eoVolumeType :: Lens' EBSOptions (Maybe VolumeType)
eoVolumeType = lens _eoVolumeType (\s a -> s {_eoVolumeType = a})

-- | Specifies whether EBS-based storage is enabled.
eoEBSEnabled :: Lens' EBSOptions (Maybe Bool)
eoEBSEnabled = lens _eoEBSEnabled (\s a -> s {_eoEBSEnabled = a})

instance FromJSON EBSOptions where
  parseJSON =
    withObject
      "EBSOptions"
      ( \x ->
          EBSOptions'
            <$> (x .:? "VolumeSize")
            <*> (x .:? "Iops")
            <*> (x .:? "VolumeType")
            <*> (x .:? "EBSEnabled")
      )

instance Hashable EBSOptions

instance NFData EBSOptions

instance ToJSON EBSOptions where
  toJSON EBSOptions' {..} =
    object
      ( catMaybes
          [ ("VolumeSize" .=) <$> _eoVolumeSize,
            ("Iops" .=) <$> _eoIOPS,
            ("VolumeType" .=) <$> _eoVolumeType,
            ("EBSEnabled" .=) <$> _eoEBSEnabled
          ]
      )
