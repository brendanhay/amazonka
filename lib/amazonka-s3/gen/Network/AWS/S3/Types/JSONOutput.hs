{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    jsonoRecordDelimiter,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.RecordDelimiter as Types

-- | Specifies JSON as request's output serialization format.
--
-- /See:/ 'mkJSONOutput' smart constructor.
newtype JSONOutput = JSONOutput'
  { -- | The value used to separate individual records in the output. If no value is specified, Amazon S3 uses a newline character ('\n').
    recordDelimiter :: Core.Maybe Types.RecordDelimiter
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'JSONOutput' value with any optional fields omitted.
mkJSONOutput ::
  JSONOutput
mkJSONOutput = JSONOutput' {recordDelimiter = Core.Nothing}

-- | The value used to separate individual records in the output. If no value is specified, Amazon S3 uses a newline character ('\n').
--
-- /Note:/ Consider using 'recordDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsonoRecordDelimiter :: Lens.Lens' JSONOutput (Core.Maybe Types.RecordDelimiter)
jsonoRecordDelimiter = Lens.field @"recordDelimiter"
{-# DEPRECATED jsonoRecordDelimiter "Use generic-lens or generic-optics with 'recordDelimiter' instead." #-}

instance Core.ToXML JSONOutput where
  toXML JSONOutput {..} =
    Core.toXMLNode "RecordDelimiter" Core.<$> recordDelimiter
