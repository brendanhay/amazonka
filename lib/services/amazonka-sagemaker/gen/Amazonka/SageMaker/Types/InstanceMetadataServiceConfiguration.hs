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
-- Module      : Amazonka.SageMaker.Types.InstanceMetadataServiceConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.InstanceMetadataServiceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information on the IMDS configuration of the notebook instance
--
-- /See:/ 'newInstanceMetadataServiceConfiguration' smart constructor.
data InstanceMetadataServiceConfiguration = InstanceMetadataServiceConfiguration'
  { -- | Indicates the minimum IMDS version that the notebook instance supports.
    -- When passed as part of @CreateNotebookInstance@, if no value is
    -- selected, then it defaults to IMDSv1. This means that both IMDSv1 and
    -- IMDSv2 are supported. If passed as part of @UpdateNotebookInstance@,
    -- there is no default.
    minimumInstanceMetadataServiceVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceMetadataServiceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minimumInstanceMetadataServiceVersion', 'instanceMetadataServiceConfiguration_minimumInstanceMetadataServiceVersion' - Indicates the minimum IMDS version that the notebook instance supports.
-- When passed as part of @CreateNotebookInstance@, if no value is
-- selected, then it defaults to IMDSv1. This means that both IMDSv1 and
-- IMDSv2 are supported. If passed as part of @UpdateNotebookInstance@,
-- there is no default.
newInstanceMetadataServiceConfiguration ::
  -- | 'minimumInstanceMetadataServiceVersion'
  Prelude.Text ->
  InstanceMetadataServiceConfiguration
newInstanceMetadataServiceConfiguration
  pMinimumInstanceMetadataServiceVersion_ =
    InstanceMetadataServiceConfiguration'
      { minimumInstanceMetadataServiceVersion =
          pMinimumInstanceMetadataServiceVersion_
      }

-- | Indicates the minimum IMDS version that the notebook instance supports.
-- When passed as part of @CreateNotebookInstance@, if no value is
-- selected, then it defaults to IMDSv1. This means that both IMDSv1 and
-- IMDSv2 are supported. If passed as part of @UpdateNotebookInstance@,
-- there is no default.
instanceMetadataServiceConfiguration_minimumInstanceMetadataServiceVersion :: Lens.Lens' InstanceMetadataServiceConfiguration Prelude.Text
instanceMetadataServiceConfiguration_minimumInstanceMetadataServiceVersion = Lens.lens (\InstanceMetadataServiceConfiguration' {minimumInstanceMetadataServiceVersion} -> minimumInstanceMetadataServiceVersion) (\s@InstanceMetadataServiceConfiguration' {} a -> s {minimumInstanceMetadataServiceVersion = a} :: InstanceMetadataServiceConfiguration)

instance
  Data.FromJSON
    InstanceMetadataServiceConfiguration
  where
  parseJSON =
    Data.withObject
      "InstanceMetadataServiceConfiguration"
      ( \x ->
          InstanceMetadataServiceConfiguration'
            Prelude.<$> (x Data..: "MinimumInstanceMetadataServiceVersion")
      )

instance
  Prelude.Hashable
    InstanceMetadataServiceConfiguration
  where
  hashWithSalt
    _salt
    InstanceMetadataServiceConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` minimumInstanceMetadataServiceVersion

instance
  Prelude.NFData
    InstanceMetadataServiceConfiguration
  where
  rnf InstanceMetadataServiceConfiguration' {..} =
    Prelude.rnf minimumInstanceMetadataServiceVersion

instance
  Data.ToJSON
    InstanceMetadataServiceConfiguration
  where
  toJSON InstanceMetadataServiceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "MinimumInstanceMetadataServiceVersion"
                  Data..= minimumInstanceMetadataServiceVersion
              )
          ]
      )
