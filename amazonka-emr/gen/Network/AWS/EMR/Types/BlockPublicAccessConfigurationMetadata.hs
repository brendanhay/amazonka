{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.BlockPublicAccessConfigurationMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.BlockPublicAccessConfigurationMetadata where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Properties that describe the AWS principal that created the
-- @BlockPublicAccessConfiguration@ using the
-- @PutBlockPublicAccessConfiguration@ action as well as the date and time
-- that the configuration was created. Each time a configuration for block
-- public access is updated, Amazon EMR updates this metadata.
--
-- /See:/ 'newBlockPublicAccessConfigurationMetadata' smart constructor.
data BlockPublicAccessConfigurationMetadata = BlockPublicAccessConfigurationMetadata'
  { -- | The date and time that the configuration was created.
    creationDateTime :: Prelude.POSIX,
    -- | The Amazon Resource Name that created or last modified the
    -- configuration.
    createdByArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BlockPublicAccessConfigurationMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'blockPublicAccessConfigurationMetadata_creationDateTime' - The date and time that the configuration was created.
--
-- 'createdByArn', 'blockPublicAccessConfigurationMetadata_createdByArn' - The Amazon Resource Name that created or last modified the
-- configuration.
newBlockPublicAccessConfigurationMetadata ::
  -- | 'creationDateTime'
  Prelude.UTCTime ->
  -- | 'createdByArn'
  Prelude.Text ->
  BlockPublicAccessConfigurationMetadata
newBlockPublicAccessConfigurationMetadata
  pCreationDateTime_
  pCreatedByArn_ =
    BlockPublicAccessConfigurationMetadata'
      { creationDateTime =
          Prelude._Time
            Lens.# pCreationDateTime_,
        createdByArn = pCreatedByArn_
      }

-- | The date and time that the configuration was created.
blockPublicAccessConfigurationMetadata_creationDateTime :: Lens.Lens' BlockPublicAccessConfigurationMetadata Prelude.UTCTime
blockPublicAccessConfigurationMetadata_creationDateTime = Lens.lens (\BlockPublicAccessConfigurationMetadata' {creationDateTime} -> creationDateTime) (\s@BlockPublicAccessConfigurationMetadata' {} a -> s {creationDateTime = a} :: BlockPublicAccessConfigurationMetadata) Prelude.. Prelude._Time

-- | The Amazon Resource Name that created or last modified the
-- configuration.
blockPublicAccessConfigurationMetadata_createdByArn :: Lens.Lens' BlockPublicAccessConfigurationMetadata Prelude.Text
blockPublicAccessConfigurationMetadata_createdByArn = Lens.lens (\BlockPublicAccessConfigurationMetadata' {createdByArn} -> createdByArn) (\s@BlockPublicAccessConfigurationMetadata' {} a -> s {createdByArn = a} :: BlockPublicAccessConfigurationMetadata)

instance
  Prelude.FromJSON
    BlockPublicAccessConfigurationMetadata
  where
  parseJSON =
    Prelude.withObject
      "BlockPublicAccessConfigurationMetadata"
      ( \x ->
          BlockPublicAccessConfigurationMetadata'
            Prelude.<$> (x Prelude..: "CreationDateTime")
            Prelude.<*> (x Prelude..: "CreatedByArn")
      )

instance
  Prelude.Hashable
    BlockPublicAccessConfigurationMetadata

instance
  Prelude.NFData
    BlockPublicAccessConfigurationMetadata
