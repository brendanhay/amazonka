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
-- Module      : Network.AWS.AlexaBusiness.Types.SipAddress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SipAddress where

import Network.AWS.AlexaBusiness.Types.SipType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The SIP address for the contact containing the URI and SIP address type.
--
-- /See:/ 'newSipAddress' smart constructor.
data SipAddress = SipAddress'
  { -- | The URI for the SIP address.
    uri :: Core.Sensitive Core.Text,
    -- | The type of the SIP address.
    type' :: Core.Sensitive SipType
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'type''
  SipType ->
  SipAddress
newSipAddress pUri_ pType_ =
  SipAddress'
    { uri = Core._Sensitive Lens.# pUri_,
      type' = Core._Sensitive Lens.# pType_
    }

-- | The URI for the SIP address.
sipAddress_uri :: Lens.Lens' SipAddress Core.Text
sipAddress_uri = Lens.lens (\SipAddress' {uri} -> uri) (\s@SipAddress' {} a -> s {uri = a} :: SipAddress) Core.. Core._Sensitive

-- | The type of the SIP address.
sipAddress_type :: Lens.Lens' SipAddress SipType
sipAddress_type = Lens.lens (\SipAddress' {type'} -> type') (\s@SipAddress' {} a -> s {type' = a} :: SipAddress) Core.. Core._Sensitive

instance Core.FromJSON SipAddress where
  parseJSON =
    Core.withObject
      "SipAddress"
      ( \x ->
          SipAddress'
            Core.<$> (x Core..: "Uri") Core.<*> (x Core..: "Type")
      )

instance Core.Hashable SipAddress

instance Core.NFData SipAddress

instance Core.ToJSON SipAddress where
  toJSON SipAddress' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Uri" Core..= uri),
            Core.Just ("Type" Core..= type')
          ]
      )
