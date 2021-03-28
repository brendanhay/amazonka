{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Source
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.Source
  ( Source (..)
  -- * Smart constructor
  , mkSource
  -- * Lenses
  , sPassword
  , sRevision
  , sSshKey
  , sType
  , sUrl
  , sUsername
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.SourceType as Types
import qualified Network.AWS.Prelude as Core

-- | Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Creating Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Custom Recipes and Cookbooks> .
--
-- /See:/ 'mkSource' smart constructor.
data Source = Source'
  { password :: Core.Maybe Core.Text
    -- ^ When included in a request, the parameter depends on the repository type.
--
--
--     * For Amazon S3 bundles, set @Password@ to the appropriate IAM secret access key.
--
--
--     * For HTTP bundles and Subversion repositories, set @Password@ to the password.
--
--
-- For more information on how to safely handle IAM credentials, see <https://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html https://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html> .
-- In responses, AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value.
  , revision :: Core.Maybe Core.Text
    -- ^ The application's version. AWS OpsWorks Stacks enables you to easily deploy new versions of an application. One of the simplest approaches is to have branches or revisions in your repository that represent different versions that can potentially be deployed.
  , sshKey :: Core.Maybe Core.Text
    -- ^ In requests, the repository's SSH key.
--
-- In responses, AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value.
  , type' :: Core.Maybe Types.SourceType
    -- ^ The repository type.
  , url :: Core.Maybe Core.Text
    -- ^ The source URL. The following is an example of an Amazon S3 source URL: @https://s3.amazonaws.com/opsworks-demo-bucket/opsworks_cookbook_demo.tar.gz@ .
  , username :: Core.Maybe Core.Text
    -- ^ This parameter depends on the repository type.
--
--
--     * For Amazon S3 bundles, set @Username@ to the appropriate IAM access key ID.
--
--
--     * For HTTP bundles, Git repositories, and Subversion repositories, set @Username@ to the user name.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Source' value with any optional fields omitted.
mkSource
    :: Source
mkSource
  = Source'{password = Core.Nothing, revision = Core.Nothing,
            sshKey = Core.Nothing, type' = Core.Nothing, url = Core.Nothing,
            username = Core.Nothing}

-- | When included in a request, the parameter depends on the repository type.
--
--
--     * For Amazon S3 bundles, set @Password@ to the appropriate IAM secret access key.
--
--
--     * For HTTP bundles and Subversion repositories, set @Password@ to the password.
--
--
-- For more information on how to safely handle IAM credentials, see <https://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html https://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html> .
-- In responses, AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPassword :: Lens.Lens' Source (Core.Maybe Core.Text)
sPassword = Lens.field @"password"
{-# INLINEABLE sPassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

-- | The application's version. AWS OpsWorks Stacks enables you to easily deploy new versions of an application. One of the simplest approaches is to have branches or revisions in your repository that represent different versions that can potentially be deployed.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRevision :: Lens.Lens' Source (Core.Maybe Core.Text)
sRevision = Lens.field @"revision"
{-# INLINEABLE sRevision #-}
{-# DEPRECATED revision "Use generic-lens or generic-optics with 'revision' instead"  #-}

-- | In requests, the repository's SSH key.
--
-- In responses, AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value.
--
-- /Note:/ Consider using 'sshKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSshKey :: Lens.Lens' Source (Core.Maybe Core.Text)
sSshKey = Lens.field @"sshKey"
{-# INLINEABLE sSshKey #-}
{-# DEPRECATED sshKey "Use generic-lens or generic-optics with 'sshKey' instead"  #-}

-- | The repository type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sType :: Lens.Lens' Source (Core.Maybe Types.SourceType)
sType = Lens.field @"type'"
{-# INLINEABLE sType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The source URL. The following is an example of an Amazon S3 source URL: @https://s3.amazonaws.com/opsworks-demo-bucket/opsworks_cookbook_demo.tar.gz@ .
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sUrl :: Lens.Lens' Source (Core.Maybe Core.Text)
sUrl = Lens.field @"url"
{-# INLINEABLE sUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

-- | This parameter depends on the repository type.
--
--
--     * For Amazon S3 bundles, set @Username@ to the appropriate IAM access key ID.
--
--
--     * For HTTP bundles, Git repositories, and Subversion repositories, set @Username@ to the user name.
--
--
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sUsername :: Lens.Lens' Source (Core.Maybe Core.Text)
sUsername = Lens.field @"username"
{-# INLINEABLE sUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.FromJSON Source where
        toJSON Source{..}
          = Core.object
              (Core.catMaybes
                 [("Password" Core..=) Core.<$> password,
                  ("Revision" Core..=) Core.<$> revision,
                  ("SshKey" Core..=) Core.<$> sshKey,
                  ("Type" Core..=) Core.<$> type', ("Url" Core..=) Core.<$> url,
                  ("Username" Core..=) Core.<$> username])

instance Core.FromJSON Source where
        parseJSON
          = Core.withObject "Source" Core.$
              \ x ->
                Source' Core.<$>
                  (x Core..:? "Password") Core.<*> x Core..:? "Revision" Core.<*>
                    x Core..:? "SshKey"
                    Core.<*> x Core..:? "Type"
                    Core.<*> x Core..:? "Url"
                    Core.<*> x Core..:? "Username"
