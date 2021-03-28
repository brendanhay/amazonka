{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.KinesisFirehoseConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.KinesisFirehoseConfig
  ( KinesisFirehoseConfig (..)
  -- * Smart constructor
  , mkKinesisFirehoseConfig
  -- * Lenses
  , kfcFirehoseArn
  ) where

import qualified Network.AWS.Connect.Types.ARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration information of a Kinesis Firehose delivery stream.
--
-- /See:/ 'mkKinesisFirehoseConfig' smart constructor.
newtype KinesisFirehoseConfig = KinesisFirehoseConfig'
  { firehoseArn :: Types.ARN
    -- ^ The Amazon Resource Name (ARN) of the delivery stream.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'KinesisFirehoseConfig' value with any optional fields omitted.
mkKinesisFirehoseConfig
    :: Types.ARN -- ^ 'firehoseArn'
    -> KinesisFirehoseConfig
mkKinesisFirehoseConfig firehoseArn
  = KinesisFirehoseConfig'{firehoseArn}

-- | The Amazon Resource Name (ARN) of the delivery stream.
--
-- /Note:/ Consider using 'firehoseArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfcFirehoseArn :: Lens.Lens' KinesisFirehoseConfig Types.ARN
kfcFirehoseArn = Lens.field @"firehoseArn"
{-# INLINEABLE kfcFirehoseArn #-}
{-# DEPRECATED firehoseArn "Use generic-lens or generic-optics with 'firehoseArn' instead"  #-}

instance Core.FromJSON KinesisFirehoseConfig where
        toJSON KinesisFirehoseConfig{..}
          = Core.object
              (Core.catMaybes [Core.Just ("FirehoseArn" Core..= firehoseArn)])

instance Core.FromJSON KinesisFirehoseConfig where
        parseJSON
          = Core.withObject "KinesisFirehoseConfig" Core.$
              \ x -> KinesisFirehoseConfig' Core.<$> (x Core..: "FirehoseArn")
