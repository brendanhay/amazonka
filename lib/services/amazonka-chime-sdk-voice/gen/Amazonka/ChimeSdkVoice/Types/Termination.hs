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
-- Module      : Amazonka.ChimeSdkVoice.Types.Termination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.Termination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newTermination' smart constructor.
data Termination = Termination'
  { callingRegions :: Prelude.Maybe [Prelude.Text],
    cidrAllowedList :: Prelude.Maybe [Prelude.Text],
    cpsLimit :: Prelude.Maybe Prelude.Natural,
    defaultPhoneNumber :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    disabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Termination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callingRegions', 'termination_callingRegions' - Undocumented member.
--
-- 'cidrAllowedList', 'termination_cidrAllowedList' - Undocumented member.
--
-- 'cpsLimit', 'termination_cpsLimit' - Undocumented member.
--
-- 'defaultPhoneNumber', 'termination_defaultPhoneNumber' - Undocumented member.
--
-- 'disabled', 'termination_disabled' - Undocumented member.
newTermination ::
  Termination
newTermination =
  Termination'
    { callingRegions = Prelude.Nothing,
      cidrAllowedList = Prelude.Nothing,
      cpsLimit = Prelude.Nothing,
      defaultPhoneNumber = Prelude.Nothing,
      disabled = Prelude.Nothing
    }

-- | Undocumented member.
termination_callingRegions :: Lens.Lens' Termination (Prelude.Maybe [Prelude.Text])
termination_callingRegions = Lens.lens (\Termination' {callingRegions} -> callingRegions) (\s@Termination' {} a -> s {callingRegions = a} :: Termination) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
termination_cidrAllowedList :: Lens.Lens' Termination (Prelude.Maybe [Prelude.Text])
termination_cidrAllowedList = Lens.lens (\Termination' {cidrAllowedList} -> cidrAllowedList) (\s@Termination' {} a -> s {cidrAllowedList = a} :: Termination) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
termination_cpsLimit :: Lens.Lens' Termination (Prelude.Maybe Prelude.Natural)
termination_cpsLimit = Lens.lens (\Termination' {cpsLimit} -> cpsLimit) (\s@Termination' {} a -> s {cpsLimit = a} :: Termination)

-- | Undocumented member.
termination_defaultPhoneNumber :: Lens.Lens' Termination (Prelude.Maybe Prelude.Text)
termination_defaultPhoneNumber = Lens.lens (\Termination' {defaultPhoneNumber} -> defaultPhoneNumber) (\s@Termination' {} a -> s {defaultPhoneNumber = a} :: Termination) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
termination_disabled :: Lens.Lens' Termination (Prelude.Maybe Prelude.Bool)
termination_disabled = Lens.lens (\Termination' {disabled} -> disabled) (\s@Termination' {} a -> s {disabled = a} :: Termination)

instance Data.FromJSON Termination where
  parseJSON =
    Data.withObject
      "Termination"
      ( \x ->
          Termination'
            Prelude.<$> (x Data..:? "CallingRegions" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "CidrAllowedList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CpsLimit")
            Prelude.<*> (x Data..:? "DefaultPhoneNumber")
            Prelude.<*> (x Data..:? "Disabled")
      )

instance Prelude.Hashable Termination where
  hashWithSalt _salt Termination' {..} =
    _salt
      `Prelude.hashWithSalt` callingRegions
      `Prelude.hashWithSalt` cidrAllowedList
      `Prelude.hashWithSalt` cpsLimit
      `Prelude.hashWithSalt` defaultPhoneNumber
      `Prelude.hashWithSalt` disabled

instance Prelude.NFData Termination where
  rnf Termination' {..} =
    Prelude.rnf callingRegions `Prelude.seq`
      Prelude.rnf cidrAllowedList `Prelude.seq`
        Prelude.rnf cpsLimit `Prelude.seq`
          Prelude.rnf defaultPhoneNumber `Prelude.seq`
            Prelude.rnf disabled

instance Data.ToJSON Termination where
  toJSON Termination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CallingRegions" Data..=)
              Prelude.<$> callingRegions,
            ("CidrAllowedList" Data..=)
              Prelude.<$> cidrAllowedList,
            ("CpsLimit" Data..=) Prelude.<$> cpsLimit,
            ("DefaultPhoneNumber" Data..=)
              Prelude.<$> defaultPhoneNumber,
            ("Disabled" Data..=) Prelude.<$> disabled
          ]
      )
