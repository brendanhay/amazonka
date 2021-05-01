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
-- Module      : Network.AWS.AlexaBusiness.Types.SipAddress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SipAddress where

import Network.AWS.AlexaBusiness.Types.SipType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The SIP address for the contact containing the URI and SIP address type.
--
-- /See:/ 'newSipAddress' smart constructor.
data SipAddress = SipAddress'
  { -- | The URI for the SIP address.
    uri :: Prelude.Sensitive Prelude.Text,
    -- | The type of the SIP address.
    type' :: Prelude.Sensitive SipType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { uri = Prelude._Sensitive Lens.# pUri_,
      type' = Prelude._Sensitive Lens.# pType_
    }

-- | The URI for the SIP address.
sipAddress_uri :: Lens.Lens' SipAddress Prelude.Text
sipAddress_uri = Lens.lens (\SipAddress' {uri} -> uri) (\s@SipAddress' {} a -> s {uri = a} :: SipAddress) Prelude.. Prelude._Sensitive

-- | The type of the SIP address.
sipAddress_type :: Lens.Lens' SipAddress SipType
sipAddress_type = Lens.lens (\SipAddress' {type'} -> type') (\s@SipAddress' {} a -> s {type' = a} :: SipAddress) Prelude.. Prelude._Sensitive

instance Prelude.FromJSON SipAddress where
  parseJSON =
    Prelude.withObject
      "SipAddress"
      ( \x ->
          SipAddress'
            Prelude.<$> (x Prelude..: "Uri")
            Prelude.<*> (x Prelude..: "Type")
      )

instance Prelude.Hashable SipAddress

instance Prelude.NFData SipAddress

instance Prelude.ToJSON SipAddress where
  toJSON SipAddress' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Uri" Prelude..= uri),
            Prelude.Just ("Type" Prelude..= type')
          ]
      )
