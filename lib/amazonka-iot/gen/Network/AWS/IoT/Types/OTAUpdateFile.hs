{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.OTAUpdateFile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.OTAUpdateFile
  ( OTAUpdateFile (..),

    -- * Smart constructor
    mkOTAUpdateFile,

    -- * Lenses
    otaufFileLocation,
    otaufFileType,
    otaufFileVersion,
    otaufAttributes,
    otaufCodeSigning,
    otaufFileName,
  )
where

import Network.AWS.IoT.Types.CodeSigning
import Network.AWS.IoT.Types.FileLocation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a file to be associated with an OTA update.
--
-- /See:/ 'mkOTAUpdateFile' smart constructor.
data OTAUpdateFile = OTAUpdateFile'
  { -- | The location of the updated firmware.
    fileLocation :: Lude.Maybe FileLocation,
    -- | An integer value you can include in the job document to allow your devices to identify the type of file received from the cloud.
    fileType :: Lude.Maybe Lude.Natural,
    -- | The file version.
    fileVersion :: Lude.Maybe Lude.Text,
    -- | A list of name/attribute pairs.
    attributes :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The code signing method of the file.
    codeSigning :: Lude.Maybe CodeSigning,
    -- | The name of the file.
    fileName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OTAUpdateFile' with the minimum fields required to make a request.
--
-- * 'fileLocation' - The location of the updated firmware.
-- * 'fileType' - An integer value you can include in the job document to allow your devices to identify the type of file received from the cloud.
-- * 'fileVersion' - The file version.
-- * 'attributes' - A list of name/attribute pairs.
-- * 'codeSigning' - The code signing method of the file.
-- * 'fileName' - The name of the file.
mkOTAUpdateFile ::
  OTAUpdateFile
mkOTAUpdateFile =
  OTAUpdateFile'
    { fileLocation = Lude.Nothing,
      fileType = Lude.Nothing,
      fileVersion = Lude.Nothing,
      attributes = Lude.Nothing,
      codeSigning = Lude.Nothing,
      fileName = Lude.Nothing
    }

-- | The location of the updated firmware.
--
-- /Note:/ Consider using 'fileLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otaufFileLocation :: Lens.Lens' OTAUpdateFile (Lude.Maybe FileLocation)
otaufFileLocation = Lens.lens (fileLocation :: OTAUpdateFile -> Lude.Maybe FileLocation) (\s a -> s {fileLocation = a} :: OTAUpdateFile)
{-# DEPRECATED otaufFileLocation "Use generic-lens or generic-optics with 'fileLocation' instead." #-}

-- | An integer value you can include in the job document to allow your devices to identify the type of file received from the cloud.
--
-- /Note:/ Consider using 'fileType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otaufFileType :: Lens.Lens' OTAUpdateFile (Lude.Maybe Lude.Natural)
otaufFileType = Lens.lens (fileType :: OTAUpdateFile -> Lude.Maybe Lude.Natural) (\s a -> s {fileType = a} :: OTAUpdateFile)
{-# DEPRECATED otaufFileType "Use generic-lens or generic-optics with 'fileType' instead." #-}

-- | The file version.
--
-- /Note:/ Consider using 'fileVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otaufFileVersion :: Lens.Lens' OTAUpdateFile (Lude.Maybe Lude.Text)
otaufFileVersion = Lens.lens (fileVersion :: OTAUpdateFile -> Lude.Maybe Lude.Text) (\s a -> s {fileVersion = a} :: OTAUpdateFile)
{-# DEPRECATED otaufFileVersion "Use generic-lens or generic-optics with 'fileVersion' instead." #-}

-- | A list of name/attribute pairs.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otaufAttributes :: Lens.Lens' OTAUpdateFile (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
otaufAttributes = Lens.lens (attributes :: OTAUpdateFile -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: OTAUpdateFile)
{-# DEPRECATED otaufAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The code signing method of the file.
--
-- /Note:/ Consider using 'codeSigning' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otaufCodeSigning :: Lens.Lens' OTAUpdateFile (Lude.Maybe CodeSigning)
otaufCodeSigning = Lens.lens (codeSigning :: OTAUpdateFile -> Lude.Maybe CodeSigning) (\s a -> s {codeSigning = a} :: OTAUpdateFile)
{-# DEPRECATED otaufCodeSigning "Use generic-lens or generic-optics with 'codeSigning' instead." #-}

-- | The name of the file.
--
-- /Note:/ Consider using 'fileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otaufFileName :: Lens.Lens' OTAUpdateFile (Lude.Maybe Lude.Text)
otaufFileName = Lens.lens (fileName :: OTAUpdateFile -> Lude.Maybe Lude.Text) (\s a -> s {fileName = a} :: OTAUpdateFile)
{-# DEPRECATED otaufFileName "Use generic-lens or generic-optics with 'fileName' instead." #-}

instance Lude.FromJSON OTAUpdateFile where
  parseJSON =
    Lude.withObject
      "OTAUpdateFile"
      ( \x ->
          OTAUpdateFile'
            Lude.<$> (x Lude..:? "fileLocation")
            Lude.<*> (x Lude..:? "fileType")
            Lude.<*> (x Lude..:? "fileVersion")
            Lude.<*> (x Lude..:? "attributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "codeSigning")
            Lude.<*> (x Lude..:? "fileName")
      )

instance Lude.ToJSON OTAUpdateFile where
  toJSON OTAUpdateFile' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("fileLocation" Lude..=) Lude.<$> fileLocation,
            ("fileType" Lude..=) Lude.<$> fileType,
            ("fileVersion" Lude..=) Lude.<$> fileVersion,
            ("attributes" Lude..=) Lude.<$> attributes,
            ("codeSigning" Lude..=) Lude.<$> codeSigning,
            ("fileName" Lude..=) Lude.<$> fileName
          ]
      )
