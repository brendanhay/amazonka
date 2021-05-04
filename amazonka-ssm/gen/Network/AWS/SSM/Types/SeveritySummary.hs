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
-- Module      : Network.AWS.SSM.Types.SeveritySummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.SeveritySummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The number of managed instances found for each patch severity level
-- defined in the request filter.
--
-- /See:/ 'newSeveritySummary' smart constructor.
data SeveritySummary = SeveritySummary'
  { -- | The total number of resources or compliance items that have a severity
    -- level of low. Low severity is determined by the organization that
    -- published the compliance items.
    lowCount :: Prelude.Maybe Prelude.Int,
    -- | The total number of resources or compliance items that have a severity
    -- level of medium. Medium severity is determined by the organization that
    -- published the compliance items.
    mediumCount :: Prelude.Maybe Prelude.Int,
    -- | The total number of resources or compliance items that have a severity
    -- level of critical. Critical severity is determined by the organization
    -- that published the compliance items.
    criticalCount :: Prelude.Maybe Prelude.Int,
    -- | The total number of resources or compliance items that have a severity
    -- level of high. High severity is determined by the organization that
    -- published the compliance items.
    highCount :: Prelude.Maybe Prelude.Int,
    -- | The total number of resources or compliance items that have a severity
    -- level of unspecified. Unspecified severity is determined by the
    -- organization that published the compliance items.
    unspecifiedCount :: Prelude.Maybe Prelude.Int,
    -- | The total number of resources or compliance items that have a severity
    -- level of informational. Informational severity is determined by the
    -- organization that published the compliance items.
    informationalCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SeveritySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lowCount', 'severitySummary_lowCount' - The total number of resources or compliance items that have a severity
-- level of low. Low severity is determined by the organization that
-- published the compliance items.
--
-- 'mediumCount', 'severitySummary_mediumCount' - The total number of resources or compliance items that have a severity
-- level of medium. Medium severity is determined by the organization that
-- published the compliance items.
--
-- 'criticalCount', 'severitySummary_criticalCount' - The total number of resources or compliance items that have a severity
-- level of critical. Critical severity is determined by the organization
-- that published the compliance items.
--
-- 'highCount', 'severitySummary_highCount' - The total number of resources or compliance items that have a severity
-- level of high. High severity is determined by the organization that
-- published the compliance items.
--
-- 'unspecifiedCount', 'severitySummary_unspecifiedCount' - The total number of resources or compliance items that have a severity
-- level of unspecified. Unspecified severity is determined by the
-- organization that published the compliance items.
--
-- 'informationalCount', 'severitySummary_informationalCount' - The total number of resources or compliance items that have a severity
-- level of informational. Informational severity is determined by the
-- organization that published the compliance items.
newSeveritySummary ::
  SeveritySummary
newSeveritySummary =
  SeveritySummary'
    { lowCount = Prelude.Nothing,
      mediumCount = Prelude.Nothing,
      criticalCount = Prelude.Nothing,
      highCount = Prelude.Nothing,
      unspecifiedCount = Prelude.Nothing,
      informationalCount = Prelude.Nothing
    }

-- | The total number of resources or compliance items that have a severity
-- level of low. Low severity is determined by the organization that
-- published the compliance items.
severitySummary_lowCount :: Lens.Lens' SeveritySummary (Prelude.Maybe Prelude.Int)
severitySummary_lowCount = Lens.lens (\SeveritySummary' {lowCount} -> lowCount) (\s@SeveritySummary' {} a -> s {lowCount = a} :: SeveritySummary)

-- | The total number of resources or compliance items that have a severity
-- level of medium. Medium severity is determined by the organization that
-- published the compliance items.
severitySummary_mediumCount :: Lens.Lens' SeveritySummary (Prelude.Maybe Prelude.Int)
severitySummary_mediumCount = Lens.lens (\SeveritySummary' {mediumCount} -> mediumCount) (\s@SeveritySummary' {} a -> s {mediumCount = a} :: SeveritySummary)

-- | The total number of resources or compliance items that have a severity
-- level of critical. Critical severity is determined by the organization
-- that published the compliance items.
severitySummary_criticalCount :: Lens.Lens' SeveritySummary (Prelude.Maybe Prelude.Int)
severitySummary_criticalCount = Lens.lens (\SeveritySummary' {criticalCount} -> criticalCount) (\s@SeveritySummary' {} a -> s {criticalCount = a} :: SeveritySummary)

-- | The total number of resources or compliance items that have a severity
-- level of high. High severity is determined by the organization that
-- published the compliance items.
severitySummary_highCount :: Lens.Lens' SeveritySummary (Prelude.Maybe Prelude.Int)
severitySummary_highCount = Lens.lens (\SeveritySummary' {highCount} -> highCount) (\s@SeveritySummary' {} a -> s {highCount = a} :: SeveritySummary)

-- | The total number of resources or compliance items that have a severity
-- level of unspecified. Unspecified severity is determined by the
-- organization that published the compliance items.
severitySummary_unspecifiedCount :: Lens.Lens' SeveritySummary (Prelude.Maybe Prelude.Int)
severitySummary_unspecifiedCount = Lens.lens (\SeveritySummary' {unspecifiedCount} -> unspecifiedCount) (\s@SeveritySummary' {} a -> s {unspecifiedCount = a} :: SeveritySummary)

-- | The total number of resources or compliance items that have a severity
-- level of informational. Informational severity is determined by the
-- organization that published the compliance items.
severitySummary_informationalCount :: Lens.Lens' SeveritySummary (Prelude.Maybe Prelude.Int)
severitySummary_informationalCount = Lens.lens (\SeveritySummary' {informationalCount} -> informationalCount) (\s@SeveritySummary' {} a -> s {informationalCount = a} :: SeveritySummary)

instance Prelude.FromJSON SeveritySummary where
  parseJSON =
    Prelude.withObject
      "SeveritySummary"
      ( \x ->
          SeveritySummary'
            Prelude.<$> (x Prelude..:? "LowCount")
            Prelude.<*> (x Prelude..:? "MediumCount")
            Prelude.<*> (x Prelude..:? "CriticalCount")
            Prelude.<*> (x Prelude..:? "HighCount")
            Prelude.<*> (x Prelude..:? "UnspecifiedCount")
            Prelude.<*> (x Prelude..:? "InformationalCount")
      )

instance Prelude.Hashable SeveritySummary

instance Prelude.NFData SeveritySummary
