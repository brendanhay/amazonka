{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteInstallationMedia
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the installation medium for a DB engine that requires an on-premises customer provided license, such as Microsoft SQL Server.
module Network.AWS.RDS.DeleteInstallationMedia
  ( -- * Creating a request
    DeleteInstallationMedia (..),
    mkDeleteInstallationMedia,

    -- ** Request lenses
    dInstallationMediaId,

    -- * Destructuring the response
    InstallationMedia (..),
    mkInstallationMedia,

    -- ** Response lenses
    imEngineVersion,
    imStatus,
    imInstallationMediaId,
    imEngineInstallationMediaPath,
    imEngine,
    imOSInstallationMediaPath,
    imCustomAvailabilityZoneId,
    imFailureCause,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteInstallationMedia' smart constructor.
newtype DeleteInstallationMedia = DeleteInstallationMedia'
  { installationMediaId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteInstallationMedia' with the minimum fields required to make a request.
--
-- * 'installationMediaId' - The installation medium ID.
mkDeleteInstallationMedia ::
  -- | 'installationMediaId'
  Lude.Text ->
  DeleteInstallationMedia
mkDeleteInstallationMedia pInstallationMediaId_ =
  DeleteInstallationMedia'
    { installationMediaId =
        pInstallationMediaId_
    }

-- | The installation medium ID.
--
-- /Note:/ Consider using 'installationMediaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dInstallationMediaId :: Lens.Lens' DeleteInstallationMedia Lude.Text
dInstallationMediaId = Lens.lens (installationMediaId :: DeleteInstallationMedia -> Lude.Text) (\s a -> s {installationMediaId = a} :: DeleteInstallationMedia)
{-# DEPRECATED dInstallationMediaId "Use generic-lens or generic-optics with 'installationMediaId' instead." #-}

instance Lude.AWSRequest DeleteInstallationMedia where
  type Rs DeleteInstallationMedia = InstallationMedia
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DeleteInstallationMediaResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders DeleteInstallationMedia where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteInstallationMedia where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteInstallationMedia where
  toQuery DeleteInstallationMedia' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteInstallationMedia" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "InstallationMediaId" Lude.=: installationMediaId
      ]
