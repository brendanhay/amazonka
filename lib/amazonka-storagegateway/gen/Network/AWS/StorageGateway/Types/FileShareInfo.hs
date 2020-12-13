{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.FileShareInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.FileShareInfo
  ( FileShareInfo (..),

    -- * Smart constructor
    mkFileShareInfo,

    -- * Lenses
    fsiFileShareStatus,
    fsiGatewayARN,
    fsiFileShareId,
    fsiFileShareARN,
    fsiFileShareType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.StorageGateway.Types.FileShareType

-- | Describes a file share.
--
-- /See:/ 'mkFileShareInfo' smart constructor.
data FileShareInfo = FileShareInfo'
  { fileShareStatus :: Lude.Maybe Lude.Text,
    gatewayARN :: Lude.Maybe Lude.Text,
    fileShareId :: Lude.Maybe Lude.Text,
    fileShareARN :: Lude.Maybe Lude.Text,
    fileShareType :: Lude.Maybe FileShareType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FileShareInfo' with the minimum fields required to make a request.
--
-- * 'fileShareStatus' -
-- * 'gatewayARN' -
-- * 'fileShareId' -
-- * 'fileShareARN' -
-- * 'fileShareType' -
mkFileShareInfo ::
  FileShareInfo
mkFileShareInfo =
  FileShareInfo'
    { fileShareStatus = Lude.Nothing,
      gatewayARN = Lude.Nothing,
      fileShareId = Lude.Nothing,
      fileShareARN = Lude.Nothing,
      fileShareType = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsiFileShareStatus :: Lens.Lens' FileShareInfo (Lude.Maybe Lude.Text)
fsiFileShareStatus = Lens.lens (fileShareStatus :: FileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {fileShareStatus = a} :: FileShareInfo)
{-# DEPRECATED fsiFileShareStatus "Use generic-lens or generic-optics with 'fileShareStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsiGatewayARN :: Lens.Lens' FileShareInfo (Lude.Maybe Lude.Text)
fsiGatewayARN = Lens.lens (gatewayARN :: FileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: FileShareInfo)
{-# DEPRECATED fsiGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsiFileShareId :: Lens.Lens' FileShareInfo (Lude.Maybe Lude.Text)
fsiFileShareId = Lens.lens (fileShareId :: FileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {fileShareId = a} :: FileShareInfo)
{-# DEPRECATED fsiFileShareId "Use generic-lens or generic-optics with 'fileShareId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsiFileShareARN :: Lens.Lens' FileShareInfo (Lude.Maybe Lude.Text)
fsiFileShareARN = Lens.lens (fileShareARN :: FileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {fileShareARN = a} :: FileShareInfo)
{-# DEPRECATED fsiFileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsiFileShareType :: Lens.Lens' FileShareInfo (Lude.Maybe FileShareType)
fsiFileShareType = Lens.lens (fileShareType :: FileShareInfo -> Lude.Maybe FileShareType) (\s a -> s {fileShareType = a} :: FileShareInfo)
{-# DEPRECATED fsiFileShareType "Use generic-lens or generic-optics with 'fileShareType' instead." #-}

instance Lude.FromJSON FileShareInfo where
  parseJSON =
    Lude.withObject
      "FileShareInfo"
      ( \x ->
          FileShareInfo'
            Lude.<$> (x Lude..:? "FileShareStatus")
            Lude.<*> (x Lude..:? "GatewayARN")
            Lude.<*> (x Lude..:? "FileShareId")
            Lude.<*> (x Lude..:? "FileShareARN")
            Lude.<*> (x Lude..:? "FileShareType")
      )
