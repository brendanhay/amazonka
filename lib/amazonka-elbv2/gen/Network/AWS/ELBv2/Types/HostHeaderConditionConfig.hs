{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.HostHeaderConditionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.HostHeaderConditionConfig
  ( HostHeaderConditionConfig (..),

    -- * Smart constructor
    mkHostHeaderConditionConfig,

    -- * Lenses
    hhccValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a host header condition.
--
-- /See:/ 'mkHostHeaderConditionConfig' smart constructor.
newtype HostHeaderConditionConfig = HostHeaderConditionConfig'
  { values ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HostHeaderConditionConfig' with the minimum fields required to make a request.
--
-- * 'values' - One or more host names. The maximum size of each name is 128 characters. The comparison is case insensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character).
--
-- If you specify multiple strings, the condition is satisfied if one of the strings matches the host name.
mkHostHeaderConditionConfig ::
  HostHeaderConditionConfig
mkHostHeaderConditionConfig =
  HostHeaderConditionConfig' {values = Lude.Nothing}

-- | One or more host names. The maximum size of each name is 128 characters. The comparison is case insensitive. The following wildcard characters are supported: * (matches 0 or more characters) and ? (matches exactly 1 character).
--
-- If you specify multiple strings, the condition is satisfied if one of the strings matches the host name.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hhccValues :: Lens.Lens' HostHeaderConditionConfig (Lude.Maybe [Lude.Text])
hhccValues = Lens.lens (values :: HostHeaderConditionConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: HostHeaderConditionConfig)
{-# DEPRECATED hhccValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Lude.FromXML HostHeaderConditionConfig where
  parseXML x =
    HostHeaderConditionConfig'
      Lude.<$> ( x Lude..@? "Values" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )

instance Lude.ToQuery HostHeaderConditionConfig where
  toQuery HostHeaderConditionConfig' {..} =
    Lude.mconcat
      [ "Values"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> values)
      ]
