{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.HTTPActionHeader
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.HTTPActionHeader
  ( HTTPActionHeader (..),

    -- * Smart constructor
    mkHTTPActionHeader,

    -- * Lenses
    httpahValue,
    httpahKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The HTTP action header.
--
-- /See:/ 'mkHTTPActionHeader' smart constructor.
data HTTPActionHeader = HTTPActionHeader'
  { -- | The HTTP header value. Substitution templates are supported.
    value :: Lude.Text,
    -- | The HTTP header key.
    key :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTPActionHeader' with the minimum fields required to make a request.
--
-- * 'value' - The HTTP header value. Substitution templates are supported.
-- * 'key' - The HTTP header key.
mkHTTPActionHeader ::
  -- | 'value'
  Lude.Text ->
  -- | 'key'
  Lude.Text ->
  HTTPActionHeader
mkHTTPActionHeader pValue_ pKey_ =
  HTTPActionHeader' {value = pValue_, key = pKey_}

-- | The HTTP header value. Substitution templates are supported.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpahValue :: Lens.Lens' HTTPActionHeader Lude.Text
httpahValue = Lens.lens (value :: HTTPActionHeader -> Lude.Text) (\s a -> s {value = a} :: HTTPActionHeader)
{-# DEPRECATED httpahValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The HTTP header key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpahKey :: Lens.Lens' HTTPActionHeader Lude.Text
httpahKey = Lens.lens (key :: HTTPActionHeader -> Lude.Text) (\s a -> s {key = a} :: HTTPActionHeader)
{-# DEPRECATED httpahKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromJSON HTTPActionHeader where
  parseJSON =
    Lude.withObject
      "HTTPActionHeader"
      ( \x ->
          HTTPActionHeader'
            Lude.<$> (x Lude..: "value") Lude.<*> (x Lude..: "key")
      )

instance Lude.ToJSON HTTPActionHeader where
  toJSON HTTPActionHeader' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("value" Lude..= value), Lude.Just ("key" Lude..= key)]
      )
