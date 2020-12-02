{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.BlockPublicAccessConfigurationMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.BlockPublicAccessConfigurationMetadata where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Properties that describe the AWS principal that created the @BlockPublicAccessConfiguration@ using the @PutBlockPublicAccessConfiguration@ action as well as the date and time that the configuration was created. Each time a configuration for block public access is updated, Amazon EMR updates this metadata.
--
--
--
-- /See:/ 'blockPublicAccessConfigurationMetadata' smart constructor.
data BlockPublicAccessConfigurationMetadata = BlockPublicAccessConfigurationMetadata'
  { _bpacmCreationDateTime ::
      !POSIX,
    _bpacmCreatedByARN ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BlockPublicAccessConfigurationMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpacmCreationDateTime' - The date and time that the configuration was created.
--
-- * 'bpacmCreatedByARN' - The Amazon Resource Name that created or last modified the configuration.
blockPublicAccessConfigurationMetadata ::
  -- | 'bpacmCreationDateTime'
  UTCTime ->
  -- | 'bpacmCreatedByARN'
  Text ->
  BlockPublicAccessConfigurationMetadata
blockPublicAccessConfigurationMetadata
  pCreationDateTime_
  pCreatedByARN_ =
    BlockPublicAccessConfigurationMetadata'
      { _bpacmCreationDateTime =
          _Time # pCreationDateTime_,
        _bpacmCreatedByARN = pCreatedByARN_
      }

-- | The date and time that the configuration was created.
bpacmCreationDateTime :: Lens' BlockPublicAccessConfigurationMetadata UTCTime
bpacmCreationDateTime = lens _bpacmCreationDateTime (\s a -> s {_bpacmCreationDateTime = a}) . _Time

-- | The Amazon Resource Name that created or last modified the configuration.
bpacmCreatedByARN :: Lens' BlockPublicAccessConfigurationMetadata Text
bpacmCreatedByARN = lens _bpacmCreatedByARN (\s a -> s {_bpacmCreatedByARN = a})

instance FromJSON BlockPublicAccessConfigurationMetadata where
  parseJSON =
    withObject
      "BlockPublicAccessConfigurationMetadata"
      ( \x ->
          BlockPublicAccessConfigurationMetadata'
            <$> (x .: "CreationDateTime") <*> (x .: "CreatedByArn")
      )

instance Hashable BlockPublicAccessConfigurationMetadata

instance NFData BlockPublicAccessConfigurationMetadata
