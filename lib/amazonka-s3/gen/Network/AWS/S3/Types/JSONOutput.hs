-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.JSONOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.JSONOutput
  ( JSONOutput (..),

    -- * Smart constructor
    mkJSONOutput,

    -- * Lenses
    joRecordDelimiter,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | Specifies JSON as request's output serialization format.
--
-- /See:/ 'mkJSONOutput' smart constructor.
newtype JSONOutput = JSONOutput'
  { recordDelimiter ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JSONOutput' with the minimum fields required to make a request.
--
-- * 'recordDelimiter' - The value used to separate individual records in the output. If no value is specified, Amazon S3 uses a newline character ('\n').
mkJSONOutput ::
  JSONOutput
mkJSONOutput = JSONOutput' {recordDelimiter = Lude.Nothing}

-- | The value used to separate individual records in the output. If no value is specified, Amazon S3 uses a newline character ('\n').
--
-- /Note:/ Consider using 'recordDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joRecordDelimiter :: Lens.Lens' JSONOutput (Lude.Maybe Lude.Text)
joRecordDelimiter = Lens.lens (recordDelimiter :: JSONOutput -> Lude.Maybe Lude.Text) (\s a -> s {recordDelimiter = a} :: JSONOutput)
{-# DEPRECATED joRecordDelimiter "Use generic-lens or generic-optics with 'recordDelimiter' instead." #-}

instance Lude.ToXML JSONOutput where
  toXML JSONOutput' {..} =
    Lude.mconcat ["RecordDelimiter" Lude.@= recordDelimiter]
