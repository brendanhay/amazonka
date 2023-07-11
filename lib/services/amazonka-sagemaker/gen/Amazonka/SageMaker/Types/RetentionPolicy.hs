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
-- Module      : Amazonka.SageMaker.Types.RetentionPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RetentionPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.RetentionType

-- | The retention policy for data stored on an Amazon Elastic File System
-- (EFS) volume.
--
-- /See:/ 'newRetentionPolicy' smart constructor.
data RetentionPolicy = RetentionPolicy'
  { -- | The default is @Retain@, which specifies to keep the data stored on the
    -- EFS volume.
    --
    -- Specify @Delete@ to delete the data stored on the EFS volume.
    homeEfsFileSystem :: Prelude.Maybe RetentionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetentionPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'homeEfsFileSystem', 'retentionPolicy_homeEfsFileSystem' - The default is @Retain@, which specifies to keep the data stored on the
-- EFS volume.
--
-- Specify @Delete@ to delete the data stored on the EFS volume.
newRetentionPolicy ::
  RetentionPolicy
newRetentionPolicy =
  RetentionPolicy'
    { homeEfsFileSystem =
        Prelude.Nothing
    }

-- | The default is @Retain@, which specifies to keep the data stored on the
-- EFS volume.
--
-- Specify @Delete@ to delete the data stored on the EFS volume.
retentionPolicy_homeEfsFileSystem :: Lens.Lens' RetentionPolicy (Prelude.Maybe RetentionType)
retentionPolicy_homeEfsFileSystem = Lens.lens (\RetentionPolicy' {homeEfsFileSystem} -> homeEfsFileSystem) (\s@RetentionPolicy' {} a -> s {homeEfsFileSystem = a} :: RetentionPolicy)

instance Prelude.Hashable RetentionPolicy where
  hashWithSalt _salt RetentionPolicy' {..} =
    _salt `Prelude.hashWithSalt` homeEfsFileSystem

instance Prelude.NFData RetentionPolicy where
  rnf RetentionPolicy' {..} =
    Prelude.rnf homeEfsFileSystem

instance Data.ToJSON RetentionPolicy where
  toJSON RetentionPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HomeEfsFileSystem" Data..=)
              Prelude.<$> homeEfsFileSystem
          ]
      )
