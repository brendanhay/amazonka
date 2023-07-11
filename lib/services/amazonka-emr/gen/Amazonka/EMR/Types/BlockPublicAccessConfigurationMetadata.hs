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
-- Module      : Amazonka.EMR.Types.BlockPublicAccessConfigurationMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.BlockPublicAccessConfigurationMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Properties that describe the Amazon Web Services principal that created
-- the @BlockPublicAccessConfiguration@ using the
-- @PutBlockPublicAccessConfiguration@ action as well as the date and time
-- that the configuration was created. Each time a configuration for block
-- public access is updated, Amazon EMR updates this metadata.
--
-- /See:/ 'newBlockPublicAccessConfigurationMetadata' smart constructor.
data BlockPublicAccessConfigurationMetadata = BlockPublicAccessConfigurationMetadata'
  { -- | The date and time that the configuration was created.
    creationDateTime :: Data.POSIX,
    -- | The Amazon Resource Name that created or last modified the
    -- configuration.
    createdByArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
          Data._Time
            Lens.# pCreationDateTime_,
        createdByArn = pCreatedByArn_
      }

-- | The date and time that the configuration was created.
blockPublicAccessConfigurationMetadata_creationDateTime :: Lens.Lens' BlockPublicAccessConfigurationMetadata Prelude.UTCTime
blockPublicAccessConfigurationMetadata_creationDateTime = Lens.lens (\BlockPublicAccessConfigurationMetadata' {creationDateTime} -> creationDateTime) (\s@BlockPublicAccessConfigurationMetadata' {} a -> s {creationDateTime = a} :: BlockPublicAccessConfigurationMetadata) Prelude.. Data._Time

-- | The Amazon Resource Name that created or last modified the
-- configuration.
blockPublicAccessConfigurationMetadata_createdByArn :: Lens.Lens' BlockPublicAccessConfigurationMetadata Prelude.Text
blockPublicAccessConfigurationMetadata_createdByArn = Lens.lens (\BlockPublicAccessConfigurationMetadata' {createdByArn} -> createdByArn) (\s@BlockPublicAccessConfigurationMetadata' {} a -> s {createdByArn = a} :: BlockPublicAccessConfigurationMetadata)

instance
  Data.FromJSON
    BlockPublicAccessConfigurationMetadata
  where
  parseJSON =
    Data.withObject
      "BlockPublicAccessConfigurationMetadata"
      ( \x ->
          BlockPublicAccessConfigurationMetadata'
            Prelude.<$> (x Data..: "CreationDateTime")
            Prelude.<*> (x Data..: "CreatedByArn")
      )

instance
  Prelude.Hashable
    BlockPublicAccessConfigurationMetadata
  where
  hashWithSalt
    _salt
    BlockPublicAccessConfigurationMetadata' {..} =
      _salt
        `Prelude.hashWithSalt` creationDateTime
        `Prelude.hashWithSalt` createdByArn

instance
  Prelude.NFData
    BlockPublicAccessConfigurationMetadata
  where
  rnf BlockPublicAccessConfigurationMetadata' {..} =
    Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf createdByArn
