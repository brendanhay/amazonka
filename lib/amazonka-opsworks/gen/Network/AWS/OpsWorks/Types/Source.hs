-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Source
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.Source
  ( Source (..),

    -- * Smart constructor
    mkSource,

    -- * Lenses
    sURL,
    sUsername,
    sSSHKey,
    sPassword,
    sType,
    sRevision,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.SourceType
import qualified Network.AWS.Prelude as Lude

-- | Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Creating Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Custom Recipes and Cookbooks> .
--
-- /See:/ 'mkSource' smart constructor.
data Source = Source'
  { url :: Lude.Maybe Lude.Text,
    username :: Lude.Maybe Lude.Text,
    sshKey :: Lude.Maybe Lude.Text,
    password :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe SourceType,
    revision :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Source' with the minimum fields required to make a request.
--
-- * 'password' - When included in a request, the parameter depends on the repository type.
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
-- * 'revision' - The application's version. AWS OpsWorks Stacks enables you to easily deploy new versions of an application. One of the simplest approaches is to have branches or revisions in your repository that represent different versions that can potentially be deployed.
-- * 'sshKey' - In requests, the repository's SSH key.
--
-- In responses, AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value.
-- * 'type'' - The repository type.
-- * 'url' - The source URL. The following is an example of an Amazon S3 source URL: @https://s3.amazonaws.com/opsworks-demo-bucket/opsworks_cookbook_demo.tar.gz@ .
-- * 'username' - This parameter depends on the repository type.
--
--
--     * For Amazon S3 bundles, set @Username@ to the appropriate IAM access key ID.
--
--
--     * For HTTP bundles, Git repositories, and Subversion repositories, set @Username@ to the user name.
mkSource ::
  Source
mkSource =
  Source'
    { url = Lude.Nothing,
      username = Lude.Nothing,
      sshKey = Lude.Nothing,
      password = Lude.Nothing,
      type' = Lude.Nothing,
      revision = Lude.Nothing
    }

-- | The source URL. The following is an example of an Amazon S3 source URL: @https://s3.amazonaws.com/opsworks-demo-bucket/opsworks_cookbook_demo.tar.gz@ .
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sURL :: Lens.Lens' Source (Lude.Maybe Lude.Text)
sURL = Lens.lens (url :: Source -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: Source)
{-# DEPRECATED sURL "Use generic-lens or generic-optics with 'url' instead." #-}

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
sUsername :: Lens.Lens' Source (Lude.Maybe Lude.Text)
sUsername = Lens.lens (username :: Source -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: Source)
{-# DEPRECATED sUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | In requests, the repository's SSH key.
--
-- In responses, AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value.
--
-- /Note:/ Consider using 'sshKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSSHKey :: Lens.Lens' Source (Lude.Maybe Lude.Text)
sSSHKey = Lens.lens (sshKey :: Source -> Lude.Maybe Lude.Text) (\s a -> s {sshKey = a} :: Source)
{-# DEPRECATED sSSHKey "Use generic-lens or generic-optics with 'sshKey' instead." #-}

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
sPassword :: Lens.Lens' Source (Lude.Maybe Lude.Text)
sPassword = Lens.lens (password :: Source -> Lude.Maybe Lude.Text) (\s a -> s {password = a} :: Source)
{-# DEPRECATED sPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | The repository type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sType :: Lens.Lens' Source (Lude.Maybe SourceType)
sType = Lens.lens (type' :: Source -> Lude.Maybe SourceType) (\s a -> s {type' = a} :: Source)
{-# DEPRECATED sType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The application's version. AWS OpsWorks Stacks enables you to easily deploy new versions of an application. One of the simplest approaches is to have branches or revisions in your repository that represent different versions that can potentially be deployed.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRevision :: Lens.Lens' Source (Lude.Maybe Lude.Text)
sRevision = Lens.lens (revision :: Source -> Lude.Maybe Lude.Text) (\s a -> s {revision = a} :: Source)
{-# DEPRECATED sRevision "Use generic-lens or generic-optics with 'revision' instead." #-}

instance Lude.FromJSON Source where
  parseJSON =
    Lude.withObject
      "Source"
      ( \x ->
          Source'
            Lude.<$> (x Lude..:? "Url")
            Lude.<*> (x Lude..:? "Username")
            Lude.<*> (x Lude..:? "SshKey")
            Lude.<*> (x Lude..:? "Password")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "Revision")
      )

instance Lude.ToJSON Source where
  toJSON Source' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Url" Lude..=) Lude.<$> url,
            ("Username" Lude..=) Lude.<$> username,
            ("SshKey" Lude..=) Lude.<$> sshKey,
            ("Password" Lude..=) Lude.<$> password,
            ("Type" Lude..=) Lude.<$> type',
            ("Revision" Lude..=) Lude.<$> revision
          ]
      )
