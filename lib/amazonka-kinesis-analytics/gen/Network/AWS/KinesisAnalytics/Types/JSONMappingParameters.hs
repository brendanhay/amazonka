{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.JSONMappingParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.JSONMappingParameters
  ( JSONMappingParameters (..),

    -- * Smart constructor
    mkJSONMappingParameters,

    -- * Lenses
    jsonmpRecordRowPath,
  )
where

import qualified Network.AWS.KinesisAnalytics.Types.RecordRowPath as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides additional mapping information when JSON is the record format on the streaming source.
--
-- /See:/ 'mkJSONMappingParameters' smart constructor.
newtype JSONMappingParameters = JSONMappingParameters'
  { -- | Path to the top-level parent that contains the records.
    recordRowPath :: Types.RecordRowPath
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'JSONMappingParameters' value with any optional fields omitted.
mkJSONMappingParameters ::
  -- | 'recordRowPath'
  Types.RecordRowPath ->
  JSONMappingParameters
mkJSONMappingParameters recordRowPath =
  JSONMappingParameters' {recordRowPath}

-- | Path to the top-level parent that contains the records.
--
-- /Note:/ Consider using 'recordRowPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsonmpRecordRowPath :: Lens.Lens' JSONMappingParameters Types.RecordRowPath
jsonmpRecordRowPath = Lens.field @"recordRowPath"
{-# DEPRECATED jsonmpRecordRowPath "Use generic-lens or generic-optics with 'recordRowPath' instead." #-}

instance Core.FromJSON JSONMappingParameters where
  toJSON JSONMappingParameters {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("RecordRowPath" Core..= recordRowPath)]
      )

instance Core.FromJSON JSONMappingParameters where
  parseJSON =
    Core.withObject "JSONMappingParameters" Core.$
      \x -> JSONMappingParameters' Core.<$> (x Core..: "RecordRowPath")
