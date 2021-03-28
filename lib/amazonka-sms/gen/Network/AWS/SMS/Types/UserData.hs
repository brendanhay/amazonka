{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.UserData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.UserData
  ( UserData (..)
  -- * Smart constructor
  , mkUserData
  -- * Lenses
  , udS3Location
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.S3Location as Types

-- | A script that runs on first launch of an Amazon EC2 instance. Used for configuring the server during launch.
--
-- /See:/ 'mkUserData' smart constructor.
newtype UserData = UserData'
  { s3Location :: Core.Maybe Types.S3Location
    -- ^ Amazon S3 location of the user-data script.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UserData' value with any optional fields omitted.
mkUserData
    :: UserData
mkUserData = UserData'{s3Location = Core.Nothing}

-- | Amazon S3 location of the user-data script.
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udS3Location :: Lens.Lens' UserData (Core.Maybe Types.S3Location)
udS3Location = Lens.field @"s3Location"
{-# INLINEABLE udS3Location #-}
{-# DEPRECATED s3Location "Use generic-lens or generic-optics with 's3Location' instead"  #-}

instance Core.FromJSON UserData where
        toJSON UserData{..}
          = Core.object
              (Core.catMaybes [("s3Location" Core..=) Core.<$> s3Location])

instance Core.FromJSON UserData where
        parseJSON
          = Core.withObject "UserData" Core.$
              \ x -> UserData' Core.<$> (x Core..:? "s3Location")
