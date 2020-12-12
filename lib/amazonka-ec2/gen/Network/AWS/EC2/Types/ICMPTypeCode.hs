{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ICMPTypeCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ICMPTypeCode
  ( ICMPTypeCode (..),

    -- * Smart constructor
    mkICMPTypeCode,

    -- * Lenses
    itcCode,
    itcType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the ICMP type and code.
--
-- /See:/ 'mkICMPTypeCode' smart constructor.
data ICMPTypeCode = ICMPTypeCode'
  { code :: Lude.Maybe Lude.Int,
    type' :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ICMPTypeCode' with the minimum fields required to make a request.
--
-- * 'code' - The ICMP code. A value of -1 means all codes for the specified ICMP type.
-- * 'type'' - The ICMP type. A value of -1 means all types.
mkICMPTypeCode ::
  ICMPTypeCode
mkICMPTypeCode =
  ICMPTypeCode' {code = Lude.Nothing, type' = Lude.Nothing}

-- | The ICMP code. A value of -1 means all codes for the specified ICMP type.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itcCode :: Lens.Lens' ICMPTypeCode (Lude.Maybe Lude.Int)
itcCode = Lens.lens (code :: ICMPTypeCode -> Lude.Maybe Lude.Int) (\s a -> s {code = a} :: ICMPTypeCode)
{-# DEPRECATED itcCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The ICMP type. A value of -1 means all types.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itcType :: Lens.Lens' ICMPTypeCode (Lude.Maybe Lude.Int)
itcType = Lens.lens (type' :: ICMPTypeCode -> Lude.Maybe Lude.Int) (\s a -> s {type' = a} :: ICMPTypeCode)
{-# DEPRECATED itcType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromXML ICMPTypeCode where
  parseXML x =
    ICMPTypeCode'
      Lude.<$> (x Lude..@? "code") Lude.<*> (x Lude..@? "type")

instance Lude.ToQuery ICMPTypeCode where
  toQuery ICMPTypeCode' {..} =
    Lude.mconcat ["Code" Lude.=: code, "Type" Lude.=: type']
