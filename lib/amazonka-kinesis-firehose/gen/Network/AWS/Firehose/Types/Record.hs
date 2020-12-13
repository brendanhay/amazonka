{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.Record
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.Record
  ( Record (..),

    -- * Smart constructor
    mkRecord,

    -- * Lenses
    rData,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The unit of data in a delivery stream.
--
-- /See:/ 'mkRecord' smart constructor.
newtype Record = Record'
  { -- | The data blob, which is base64-encoded when the blob is serialized. The maximum size of the data blob, before base64-encoding, is 1,000 KiB.
    data' :: Lude.Base64
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Record' with the minimum fields required to make a request.
--
-- * 'data'' - The data blob, which is base64-encoded when the blob is serialized. The maximum size of the data blob, before base64-encoding, is 1,000 KiB.
mkRecord ::
  -- | 'data''
  Lude.Base64 ->
  Record
mkRecord pData_ = Record' {data' = pData_}

-- | The data blob, which is base64-encoded when the blob is serialized. The maximum size of the data blob, before base64-encoding, is 1,000 KiB.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rData :: Lens.Lens' Record Lude.Base64
rData = Lens.lens (data' :: Record -> Lude.Base64) (\s a -> s {data' = a} :: Record)
{-# DEPRECATED rData "Use generic-lens or generic-optics with 'data'' instead." #-}

instance Lude.ToJSON Record where
  toJSON Record' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Data" Lude..= data')])
