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
-- Module      : Network.AWS.SageMaker.Types.RetentionPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.RetentionPolicy where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.RetentionType

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.Hashable RetentionPolicy

instance Prelude.NFData RetentionPolicy

instance Prelude.ToJSON RetentionPolicy where
  toJSON RetentionPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("HomeEfsFileSystem" Prelude..=)
              Prelude.<$> homeEfsFileSystem
          ]
      )
