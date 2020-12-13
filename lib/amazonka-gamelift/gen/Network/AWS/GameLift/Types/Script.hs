{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.Script
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.Script
  ( Script (..),

    -- * Smart constructor
    mkScript,

    -- * Lenses
    sCreationTime,
    sStorageLocation,
    sName,
    sScriptId,
    sVersion,
    sScriptARN,
    sSizeOnDisk,
  )
where

import Network.AWS.GameLift.Types.S3Location
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { -- | A time stamp indicating when this data object was created. The format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
    creationTime :: Lude.Maybe Lude.Timestamp,
    storageLocation :: Lude.Maybe S3Location,
    -- | A descriptive label that is associated with a script. Script names do not need to be unique.
    name :: Lude.Maybe Lude.Text,
    -- | A unique identifier for a Realtime script
    scriptId :: Lude.Maybe Lude.Text,
    -- | The version that is associated with a build or script. Version strings do not need to be unique.
    version :: Lude.Maybe Lude.Text,
    -- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift script resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift script ARN, the resource ID matches the /ScriptId/ value.
    scriptARN :: Lude.Maybe Lude.Text,
    -- | The file size of the uploaded Realtime script, expressed in bytes. When files are uploaded from an S3 location, this value remains at "0".
    sizeOnDisk :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Script' with the minimum fields required to make a request.
--
-- * 'creationTime' - A time stamp indicating when this data object was created. The format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
-- * 'storageLocation' -
-- * 'name' - A descriptive label that is associated with a script. Script names do not need to be unique.
-- * 'scriptId' - A unique identifier for a Realtime script
-- * 'version' - The version that is associated with a build or script. Version strings do not need to be unique.
-- * 'scriptARN' - Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift script resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift script ARN, the resource ID matches the /ScriptId/ value.
-- * 'sizeOnDisk' - The file size of the uploaded Realtime script, expressed in bytes. When files are uploaded from an S3 location, this value remains at "0".
mkScript ::
  Script
mkScript =
  Script'
    { creationTime = Lude.Nothing,
      storageLocation = Lude.Nothing,
      name = Lude.Nothing,
      scriptId = Lude.Nothing,
      version = Lude.Nothing,
      scriptARN = Lude.Nothing,
      sizeOnDisk = Lude.Nothing
    }

-- | A time stamp indicating when this data object was created. The format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCreationTime :: Lens.Lens' Script (Lude.Maybe Lude.Timestamp)
sCreationTime = Lens.lens (creationTime :: Script -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: Script)
{-# DEPRECATED sCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'storageLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStorageLocation :: Lens.Lens' Script (Lude.Maybe S3Location)
sStorageLocation = Lens.lens (storageLocation :: Script -> Lude.Maybe S3Location) (\s a -> s {storageLocation = a} :: Script)
{-# DEPRECATED sStorageLocation "Use generic-lens or generic-optics with 'storageLocation' instead." #-}

-- | A descriptive label that is associated with a script. Script names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' Script (Lude.Maybe Lude.Text)
sName = Lens.lens (name :: Script -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Script)
{-# DEPRECATED sName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A unique identifier for a Realtime script
--
-- /Note:/ Consider using 'scriptId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sScriptId :: Lens.Lens' Script (Lude.Maybe Lude.Text)
sScriptId = Lens.lens (scriptId :: Script -> Lude.Maybe Lude.Text) (\s a -> s {scriptId = a} :: Script)
{-# DEPRECATED sScriptId "Use generic-lens or generic-optics with 'scriptId' instead." #-}

-- | The version that is associated with a build or script. Version strings do not need to be unique.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVersion :: Lens.Lens' Script (Lude.Maybe Lude.Text)
sVersion = Lens.lens (version :: Script -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: Script)
{-# DEPRECATED sVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift script resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift script ARN, the resource ID matches the /ScriptId/ value.
--
-- /Note:/ Consider using 'scriptARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sScriptARN :: Lens.Lens' Script (Lude.Maybe Lude.Text)
sScriptARN = Lens.lens (scriptARN :: Script -> Lude.Maybe Lude.Text) (\s a -> s {scriptARN = a} :: Script)
{-# DEPRECATED sScriptARN "Use generic-lens or generic-optics with 'scriptARN' instead." #-}

-- | The file size of the uploaded Realtime script, expressed in bytes. When files are uploaded from an S3 location, this value remains at "0".
--
-- /Note:/ Consider using 'sizeOnDisk' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSizeOnDisk :: Lens.Lens' Script (Lude.Maybe Lude.Natural)
sSizeOnDisk = Lens.lens (sizeOnDisk :: Script -> Lude.Maybe Lude.Natural) (\s a -> s {sizeOnDisk = a} :: Script)
{-# DEPRECATED sSizeOnDisk "Use generic-lens or generic-optics with 'sizeOnDisk' instead." #-}

instance Lude.FromJSON Script where
  parseJSON =
    Lude.withObject
      "Script"
      ( \x ->
          Script'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "StorageLocation")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "ScriptId")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "ScriptArn")
            Lude.<*> (x Lude..:? "SizeOnDisk")
      )
