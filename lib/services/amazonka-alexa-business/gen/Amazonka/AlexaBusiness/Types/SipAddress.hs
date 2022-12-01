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
-- Module      : Amazonka.AlexaBusiness.Types.SipAddress
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.SipAddress where

import Amazonka.AlexaBusiness.Types.SipType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The SIP address for the contact containing the URI and SIP address type.
--
-- /See:/ 'newSipAddress' smart constructor.
data SipAddress = SipAddress'
  { -- | The URI for the SIP address.
    uri :: Core.Sensitive Prelude.Text,
    -- | The type of the SIP address.
    type' :: Core.Sensitive SipType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SipAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uri', 'sipAddress_uri' - The URI for the SIP address.
--
-- 'type'', 'sipAddress_type' - The type of the SIP address.
newSipAddress ::
  -- | 'uri'
  Prelude.Text ->
  -- | 'type''
  SipType ->
  SipAddress
newSipAddress pUri_ pType_ =
  SipAddress'
    { uri = Core._Sensitive Lens.# pUri_,
      type' = Core._Sensitive Lens.# pType_
    }

-- | The URI for the SIP address.
sipAddress_uri :: Lens.Lens' SipAddress Prelude.Text
sipAddress_uri = Lens.lens (\SipAddress' {uri} -> uri) (\s@SipAddress' {} a -> s {uri = a} :: SipAddress) Prelude.. Core._Sensitive

-- | The type of the SIP address.
sipAddress_type :: Lens.Lens' SipAddress SipType
sipAddress_type = Lens.lens (\SipAddress' {type'} -> type') (\s@SipAddress' {} a -> s {type' = a} :: SipAddress) Prelude.. Core._Sensitive

instance Core.FromJSON SipAddress where
  parseJSON =
    Core.withObject
      "SipAddress"
      ( \x ->
          SipAddress'
            Prelude.<$> (x Core..: "Uri") Prelude.<*> (x Core..: "Type")
      )

instance Prelude.Hashable SipAddress where
  hashWithSalt _salt SipAddress' {..} =
    _salt `Prelude.hashWithSalt` uri
      `Prelude.hashWithSalt` type'

instance Prelude.NFData SipAddress where
  rnf SipAddress' {..} =
    Prelude.rnf uri `Prelude.seq` Prelude.rnf type'

instance Core.ToJSON SipAddress where
  toJSON SipAddress' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Uri" Core..= uri),
            Prelude.Just ("Type" Core..= type')
          ]
      )
