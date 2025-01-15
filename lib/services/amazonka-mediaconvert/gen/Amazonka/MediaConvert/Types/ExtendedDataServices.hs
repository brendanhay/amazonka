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
-- Module      : Amazonka.MediaConvert.Types.ExtendedDataServices
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.ExtendedDataServices where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.CopyProtectionAction
import Amazonka.MediaConvert.Types.VchipAction
import qualified Amazonka.Prelude as Prelude

-- | If your source content has EIA-608 Line 21 Data Services, enable this
-- feature to specify what MediaConvert does with the Extended Data
-- Services (XDS) packets. You can choose to pass through XDS packets, or
-- remove them from the output. For more information about XDS, see EIA-608
-- Line Data Services, section 9.5.1.5 05h Content Advisory.
--
-- /See:/ 'newExtendedDataServices' smart constructor.
data ExtendedDataServices = ExtendedDataServices'
  { -- | The action to take on copy and redistribution control XDS packets. If
    -- you select PASSTHROUGH, packets will not be changed. If you select
    -- STRIP, any packets will be removed in output captions.
    copyProtectionAction :: Prelude.Maybe CopyProtectionAction,
    -- | The action to take on content advisory XDS packets. If you select
    -- PASSTHROUGH, packets will not be changed. If you select STRIP, any
    -- packets will be removed in output captions.
    vchipAction :: Prelude.Maybe VchipAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExtendedDataServices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'copyProtectionAction', 'extendedDataServices_copyProtectionAction' - The action to take on copy and redistribution control XDS packets. If
-- you select PASSTHROUGH, packets will not be changed. If you select
-- STRIP, any packets will be removed in output captions.
--
-- 'vchipAction', 'extendedDataServices_vchipAction' - The action to take on content advisory XDS packets. If you select
-- PASSTHROUGH, packets will not be changed. If you select STRIP, any
-- packets will be removed in output captions.
newExtendedDataServices ::
  ExtendedDataServices
newExtendedDataServices =
  ExtendedDataServices'
    { copyProtectionAction =
        Prelude.Nothing,
      vchipAction = Prelude.Nothing
    }

-- | The action to take on copy and redistribution control XDS packets. If
-- you select PASSTHROUGH, packets will not be changed. If you select
-- STRIP, any packets will be removed in output captions.
extendedDataServices_copyProtectionAction :: Lens.Lens' ExtendedDataServices (Prelude.Maybe CopyProtectionAction)
extendedDataServices_copyProtectionAction = Lens.lens (\ExtendedDataServices' {copyProtectionAction} -> copyProtectionAction) (\s@ExtendedDataServices' {} a -> s {copyProtectionAction = a} :: ExtendedDataServices)

-- | The action to take on content advisory XDS packets. If you select
-- PASSTHROUGH, packets will not be changed. If you select STRIP, any
-- packets will be removed in output captions.
extendedDataServices_vchipAction :: Lens.Lens' ExtendedDataServices (Prelude.Maybe VchipAction)
extendedDataServices_vchipAction = Lens.lens (\ExtendedDataServices' {vchipAction} -> vchipAction) (\s@ExtendedDataServices' {} a -> s {vchipAction = a} :: ExtendedDataServices)

instance Data.FromJSON ExtendedDataServices where
  parseJSON =
    Data.withObject
      "ExtendedDataServices"
      ( \x ->
          ExtendedDataServices'
            Prelude.<$> (x Data..:? "copyProtectionAction")
            Prelude.<*> (x Data..:? "vchipAction")
      )

instance Prelude.Hashable ExtendedDataServices where
  hashWithSalt _salt ExtendedDataServices' {..} =
    _salt
      `Prelude.hashWithSalt` copyProtectionAction
      `Prelude.hashWithSalt` vchipAction

instance Prelude.NFData ExtendedDataServices where
  rnf ExtendedDataServices' {..} =
    Prelude.rnf copyProtectionAction `Prelude.seq`
      Prelude.rnf vchipAction

instance Data.ToJSON ExtendedDataServices where
  toJSON ExtendedDataServices' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("copyProtectionAction" Data..=)
              Prelude.<$> copyProtectionAction,
            ("vchipAction" Data..=) Prelude.<$> vchipAction
          ]
      )
