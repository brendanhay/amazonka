{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.Script
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.Script
  ( Script (..)
  -- * Smart constructor
  , mkScript
  -- * Lenses
  , sCreationTime
  , sName
  , sScriptArn
  , sScriptId
  , sSizeOnDisk
  , sStorageLocation
  , sVersion
  ) where

import qualified Network.AWS.GameLift.Types.Name as Types
import qualified Network.AWS.GameLift.Types.S3Location as Types
import qualified Network.AWS.GameLift.Types.ScriptArn as Types
import qualified Network.AWS.GameLift.Types.ScriptId as Types
import qualified Network.AWS.GameLift.Types.Version as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Properties describing a Realtime script.
--
-- __Related operations__ 
--
--     * 'CreateScript' 
--
--
--     * 'ListScripts' 
--
--
--     * 'DescribeScript' 
--
--
--     * 'UpdateScript' 
--
--
--     * 'DeleteScript' 
--
--
--
-- /See:/ 'mkScript' smart constructor.
data Script = Script'
  { creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A time stamp indicating when this data object was created. The format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
  , name :: Core.Maybe Types.Name
    -- ^ A descriptive label that is associated with a script. Script names do not need to be unique.
  , scriptArn :: Core.Maybe Types.ScriptArn
    -- ^ Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift script resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift script ARN, the resource ID matches the /ScriptId/ value.
  , scriptId :: Core.Maybe Types.ScriptId
    -- ^ A unique identifier for a Realtime script
  , sizeOnDisk :: Core.Maybe Core.Natural
    -- ^ The file size of the uploaded Realtime script, expressed in bytes. When files are uploaded from an S3 location, this value remains at "0".
  , storageLocation :: Core.Maybe Types.S3Location
  , version :: Core.Maybe Types.Version
    -- ^ The version that is associated with a build or script. Version strings do not need to be unique.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Script' value with any optional fields omitted.
mkScript
    :: Script
mkScript
  = Script'{creationTime = Core.Nothing, name = Core.Nothing,
            scriptArn = Core.Nothing, scriptId = Core.Nothing,
            sizeOnDisk = Core.Nothing, storageLocation = Core.Nothing,
            version = Core.Nothing}

-- | A time stamp indicating when this data object was created. The format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCreationTime :: Lens.Lens' Script (Core.Maybe Core.NominalDiffTime)
sCreationTime = Lens.field @"creationTime"
{-# INLINEABLE sCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | A descriptive label that is associated with a script. Script names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' Script (Core.Maybe Types.Name)
sName = Lens.field @"name"
{-# INLINEABLE sName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift script resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift script ARN, the resource ID matches the /ScriptId/ value.
--
-- /Note:/ Consider using 'scriptArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sScriptArn :: Lens.Lens' Script (Core.Maybe Types.ScriptArn)
sScriptArn = Lens.field @"scriptArn"
{-# INLINEABLE sScriptArn #-}
{-# DEPRECATED scriptArn "Use generic-lens or generic-optics with 'scriptArn' instead"  #-}

-- | A unique identifier for a Realtime script
--
-- /Note:/ Consider using 'scriptId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sScriptId :: Lens.Lens' Script (Core.Maybe Types.ScriptId)
sScriptId = Lens.field @"scriptId"
{-# INLINEABLE sScriptId #-}
{-# DEPRECATED scriptId "Use generic-lens or generic-optics with 'scriptId' instead"  #-}

-- | The file size of the uploaded Realtime script, expressed in bytes. When files are uploaded from an S3 location, this value remains at "0".
--
-- /Note:/ Consider using 'sizeOnDisk' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSizeOnDisk :: Lens.Lens' Script (Core.Maybe Core.Natural)
sSizeOnDisk = Lens.field @"sizeOnDisk"
{-# INLINEABLE sSizeOnDisk #-}
{-# DEPRECATED sizeOnDisk "Use generic-lens or generic-optics with 'sizeOnDisk' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'storageLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStorageLocation :: Lens.Lens' Script (Core.Maybe Types.S3Location)
sStorageLocation = Lens.field @"storageLocation"
{-# INLINEABLE sStorageLocation #-}
{-# DEPRECATED storageLocation "Use generic-lens or generic-optics with 'storageLocation' instead"  #-}

-- | The version that is associated with a build or script. Version strings do not need to be unique.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVersion :: Lens.Lens' Script (Core.Maybe Types.Version)
sVersion = Lens.field @"version"
{-# INLINEABLE sVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON Script where
        parseJSON
          = Core.withObject "Script" Core.$
              \ x ->
                Script' Core.<$>
                  (x Core..:? "CreationTime") Core.<*> x Core..:? "Name" Core.<*>
                    x Core..:? "ScriptArn"
                    Core.<*> x Core..:? "ScriptId"
                    Core.<*> x Core..:? "SizeOnDisk"
                    Core.<*> x Core..:? "StorageLocation"
                    Core.<*> x Core..:? "Version"
