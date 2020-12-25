{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.PutItemInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.PutItemInput
  ( PutItemInput (..),

    -- * Smart constructor
    mkPutItemInput,

    -- * Lenses
    piiTableName,
  )
where

import qualified Network.AWS.IoT.Types.TableName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The input for the DynamoActionVS action that specifies the DynamoDB table to which the message data will be written.
--
-- /See:/ 'mkPutItemInput' smart constructor.
newtype PutItemInput = PutItemInput'
  { -- | The table where the message data will be written.
    tableName :: Types.TableName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutItemInput' value with any optional fields omitted.
mkPutItemInput ::
  -- | 'tableName'
  Types.TableName ->
  PutItemInput
mkPutItemInput tableName = PutItemInput' {tableName}

-- | The table where the message data will be written.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piiTableName :: Lens.Lens' PutItemInput Types.TableName
piiTableName = Lens.field @"tableName"
{-# DEPRECATED piiTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Core.FromJSON PutItemInput where
  toJSON PutItemInput {..} =
    Core.object
      (Core.catMaybes [Core.Just ("tableName" Core..= tableName)])

instance Core.FromJSON PutItemInput where
  parseJSON =
    Core.withObject "PutItemInput" Core.$
      \x -> PutItemInput' Core.<$> (x Core..: "tableName")
