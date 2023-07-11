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
-- Module      : Amazonka.WellArchitected.Types.LensSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.LensSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.LensStatus
import Amazonka.WellArchitected.Types.LensType

-- | A lens summary of a lens.
--
-- /See:/ 'newLensSummary' smart constructor.
data LensSummary = LensSummary'
  { createdAt :: Prelude.Maybe Data.POSIX,
    description :: Prelude.Maybe Prelude.Text,
    lensAlias :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the lens.
    lensArn :: Prelude.Maybe Prelude.Text,
    lensName :: Prelude.Maybe Prelude.Text,
    -- | The status of the lens.
    lensStatus :: Prelude.Maybe LensStatus,
    -- | The type of the lens.
    lensType :: Prelude.Maybe LensType,
    -- | The version of the lens.
    lensVersion :: Prelude.Maybe Prelude.Text,
    owner :: Prelude.Maybe Prelude.Text,
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LensSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'lensSummary_createdAt' - Undocumented member.
--
-- 'description', 'lensSummary_description' - Undocumented member.
--
-- 'lensAlias', 'lensSummary_lensAlias' - Undocumented member.
--
-- 'lensArn', 'lensSummary_lensArn' - The ARN of the lens.
--
-- 'lensName', 'lensSummary_lensName' - Undocumented member.
--
-- 'lensStatus', 'lensSummary_lensStatus' - The status of the lens.
--
-- 'lensType', 'lensSummary_lensType' - The type of the lens.
--
-- 'lensVersion', 'lensSummary_lensVersion' - The version of the lens.
--
-- 'owner', 'lensSummary_owner' - Undocumented member.
--
-- 'updatedAt', 'lensSummary_updatedAt' - Undocumented member.
newLensSummary ::
  LensSummary
newLensSummary =
  LensSummary'
    { createdAt = Prelude.Nothing,
      description = Prelude.Nothing,
      lensAlias = Prelude.Nothing,
      lensArn = Prelude.Nothing,
      lensName = Prelude.Nothing,
      lensStatus = Prelude.Nothing,
      lensType = Prelude.Nothing,
      lensVersion = Prelude.Nothing,
      owner = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | Undocumented member.
lensSummary_createdAt :: Lens.Lens' LensSummary (Prelude.Maybe Prelude.UTCTime)
lensSummary_createdAt = Lens.lens (\LensSummary' {createdAt} -> createdAt) (\s@LensSummary' {} a -> s {createdAt = a} :: LensSummary) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
lensSummary_description :: Lens.Lens' LensSummary (Prelude.Maybe Prelude.Text)
lensSummary_description = Lens.lens (\LensSummary' {description} -> description) (\s@LensSummary' {} a -> s {description = a} :: LensSummary)

-- | Undocumented member.
lensSummary_lensAlias :: Lens.Lens' LensSummary (Prelude.Maybe Prelude.Text)
lensSummary_lensAlias = Lens.lens (\LensSummary' {lensAlias} -> lensAlias) (\s@LensSummary' {} a -> s {lensAlias = a} :: LensSummary)

-- | The ARN of the lens.
lensSummary_lensArn :: Lens.Lens' LensSummary (Prelude.Maybe Prelude.Text)
lensSummary_lensArn = Lens.lens (\LensSummary' {lensArn} -> lensArn) (\s@LensSummary' {} a -> s {lensArn = a} :: LensSummary)

-- | Undocumented member.
lensSummary_lensName :: Lens.Lens' LensSummary (Prelude.Maybe Prelude.Text)
lensSummary_lensName = Lens.lens (\LensSummary' {lensName} -> lensName) (\s@LensSummary' {} a -> s {lensName = a} :: LensSummary)

-- | The status of the lens.
lensSummary_lensStatus :: Lens.Lens' LensSummary (Prelude.Maybe LensStatus)
lensSummary_lensStatus = Lens.lens (\LensSummary' {lensStatus} -> lensStatus) (\s@LensSummary' {} a -> s {lensStatus = a} :: LensSummary)

-- | The type of the lens.
lensSummary_lensType :: Lens.Lens' LensSummary (Prelude.Maybe LensType)
lensSummary_lensType = Lens.lens (\LensSummary' {lensType} -> lensType) (\s@LensSummary' {} a -> s {lensType = a} :: LensSummary)

-- | The version of the lens.
lensSummary_lensVersion :: Lens.Lens' LensSummary (Prelude.Maybe Prelude.Text)
lensSummary_lensVersion = Lens.lens (\LensSummary' {lensVersion} -> lensVersion) (\s@LensSummary' {} a -> s {lensVersion = a} :: LensSummary)

-- | Undocumented member.
lensSummary_owner :: Lens.Lens' LensSummary (Prelude.Maybe Prelude.Text)
lensSummary_owner = Lens.lens (\LensSummary' {owner} -> owner) (\s@LensSummary' {} a -> s {owner = a} :: LensSummary)

-- | Undocumented member.
lensSummary_updatedAt :: Lens.Lens' LensSummary (Prelude.Maybe Prelude.UTCTime)
lensSummary_updatedAt = Lens.lens (\LensSummary' {updatedAt} -> updatedAt) (\s@LensSummary' {} a -> s {updatedAt = a} :: LensSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON LensSummary where
  parseJSON =
    Data.withObject
      "LensSummary"
      ( \x ->
          LensSummary'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "LensAlias")
            Prelude.<*> (x Data..:? "LensArn")
            Prelude.<*> (x Data..:? "LensName")
            Prelude.<*> (x Data..:? "LensStatus")
            Prelude.<*> (x Data..:? "LensType")
            Prelude.<*> (x Data..:? "LensVersion")
            Prelude.<*> (x Data..:? "Owner")
            Prelude.<*> (x Data..:? "UpdatedAt")
      )

instance Prelude.Hashable LensSummary where
  hashWithSalt _salt LensSummary' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lensAlias
      `Prelude.hashWithSalt` lensArn
      `Prelude.hashWithSalt` lensName
      `Prelude.hashWithSalt` lensStatus
      `Prelude.hashWithSalt` lensType
      `Prelude.hashWithSalt` lensVersion
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData LensSummary where
  rnf LensSummary' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lensAlias
      `Prelude.seq` Prelude.rnf lensArn
      `Prelude.seq` Prelude.rnf lensName
      `Prelude.seq` Prelude.rnf lensStatus
      `Prelude.seq` Prelude.rnf lensType
      `Prelude.seq` Prelude.rnf lensVersion
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf updatedAt
