-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.JSONInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.JSONInput
  ( JSONInput (..),

    -- * Smart constructor
    mkJSONInput,

    -- * Lenses
    jiType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.JSONType

-- | Specifies JSON as object's input serialization format.
--
-- /See:/ 'mkJSONInput' smart constructor.
newtype JSONInput = JSONInput' {type' :: Lude.Maybe JSONType}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JSONInput' with the minimum fields required to make a request.
--
-- * 'type'' - The type of JSON. Valid values: Document, Lines.
mkJSONInput ::
  JSONInput
mkJSONInput = JSONInput' {type' = Lude.Nothing}

-- | The type of JSON. Valid values: Document, Lines.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jiType :: Lens.Lens' JSONInput (Lude.Maybe JSONType)
jiType = Lens.lens (type' :: JSONInput -> Lude.Maybe JSONType) (\s a -> s {type' = a} :: JSONInput)
{-# DEPRECATED jiType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.ToXML JSONInput where
  toXML JSONInput' {..} = Lude.mconcat ["Type" Lude.@= type']
