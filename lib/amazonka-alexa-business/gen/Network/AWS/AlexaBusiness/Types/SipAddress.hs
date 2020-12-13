{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.SipAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SipAddress
  ( SipAddress (..),

    -- * Smart constructor
    mkSipAddress,

    -- * Lenses
    saURI,
    saType,
  )
where

import Network.AWS.AlexaBusiness.Types.SipType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The SIP address for the contact containing the URI and SIP address type.
--
-- /See:/ 'mkSipAddress' smart constructor.
data SipAddress = SipAddress'
  { -- | The URI for the SIP address.
    uri :: Lude.Sensitive Lude.Text,
    -- | The type of the SIP address.
    type' :: SipType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SipAddress' with the minimum fields required to make a request.
--
-- * 'uri' - The URI for the SIP address.
-- * 'type'' - The type of the SIP address.
mkSipAddress ::
  -- | 'uri'
  Lude.Sensitive Lude.Text ->
  -- | 'type''
  SipType ->
  SipAddress
mkSipAddress pURI_ pType_ =
  SipAddress' {uri = pURI_, type' = pType_}

-- | The URI for the SIP address.
--
-- /Note:/ Consider using 'uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saURI :: Lens.Lens' SipAddress (Lude.Sensitive Lude.Text)
saURI = Lens.lens (uri :: SipAddress -> Lude.Sensitive Lude.Text) (\s a -> s {uri = a} :: SipAddress)
{-# DEPRECATED saURI "Use generic-lens or generic-optics with 'uri' instead." #-}

-- | The type of the SIP address.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saType :: Lens.Lens' SipAddress SipType
saType = Lens.lens (type' :: SipAddress -> SipType) (\s a -> s {type' = a} :: SipAddress)
{-# DEPRECATED saType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON SipAddress where
  parseJSON =
    Lude.withObject
      "SipAddress"
      ( \x ->
          SipAddress' Lude.<$> (x Lude..: "Uri") Lude.<*> (x Lude..: "Type")
      )

instance Lude.ToJSON SipAddress where
  toJSON SipAddress' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("Uri" Lude..= uri), Lude.Just ("Type" Lude..= type')]
      )
