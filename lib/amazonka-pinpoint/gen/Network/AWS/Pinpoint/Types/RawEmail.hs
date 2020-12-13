{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.RawEmail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.RawEmail
  ( RawEmail (..),

    -- * Smart constructor
    mkRawEmail,

    -- * Lenses
    reData,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the contents of an email message, represented as a raw MIME message.
--
-- /See:/ 'mkRawEmail' smart constructor.
newtype RawEmail = RawEmail'
  { -- | The email message, represented as a raw MIME message. The entire message must be base64 encoded.
    data' :: Lude.Maybe Lude.Base64
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RawEmail' with the minimum fields required to make a request.
--
-- * 'data'' - The email message, represented as a raw MIME message. The entire message must be base64 encoded.
mkRawEmail ::
  RawEmail
mkRawEmail = RawEmail' {data' = Lude.Nothing}

-- | The email message, represented as a raw MIME message. The entire message must be base64 encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reData :: Lens.Lens' RawEmail (Lude.Maybe Lude.Base64)
reData = Lens.lens (data' :: RawEmail -> Lude.Maybe Lude.Base64) (\s a -> s {data' = a} :: RawEmail)
{-# DEPRECATED reData "Use generic-lens or generic-optics with 'data'' instead." #-}

instance Lude.ToJSON RawEmail where
  toJSON RawEmail' {..} =
    Lude.object (Lude.catMaybes [("Data" Lude..=) Lude.<$> data'])
