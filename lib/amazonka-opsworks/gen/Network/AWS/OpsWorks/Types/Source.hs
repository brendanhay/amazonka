{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Source
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.Source where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types.SourceType
import Network.AWS.Prelude

-- | Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Creating Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Custom Recipes and Cookbooks> .
--
--
--
-- /See:/ 'source' smart constructor.
data Source = Source'
  { _sURL :: !(Maybe Text),
    _sUsername :: !(Maybe Text),
    _sSSHKey :: !(Maybe Text),
    _sPassword :: !(Maybe Text),
    _sType :: !(Maybe SourceType),
    _sRevision :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Source' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sURL' - The source URL. The following is an example of an Amazon S3 source URL: @https://s3.amazonaws.com/opsworks-demo-bucket/opsworks_cookbook_demo.tar.gz@ .
--
-- * 'sUsername' - This parameter depends on the repository type.     * For Amazon S3 bundles, set @Username@ to the appropriate IAM access key ID.     * For HTTP bundles, Git repositories, and Subversion repositories, set @Username@ to the user name.
--
-- * 'sSSHKey' - In requests, the repository's SSH key. In responses, AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value.
--
-- * 'sPassword' - When included in a request, the parameter depends on the repository type.     * For Amazon S3 bundles, set @Password@ to the appropriate IAM secret access key.     * For HTTP bundles and Subversion repositories, set @Password@ to the password. For more information on how to safely handle IAM credentials, see <https://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html https://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html> . In responses, AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value.
--
-- * 'sType' - The repository type.
--
-- * 'sRevision' - The application's version. AWS OpsWorks Stacks enables you to easily deploy new versions of an application. One of the simplest approaches is to have branches or revisions in your repository that represent different versions that can potentially be deployed.
source ::
  Source
source =
  Source'
    { _sURL = Nothing,
      _sUsername = Nothing,
      _sSSHKey = Nothing,
      _sPassword = Nothing,
      _sType = Nothing,
      _sRevision = Nothing
    }

-- | The source URL. The following is an example of an Amazon S3 source URL: @https://s3.amazonaws.com/opsworks-demo-bucket/opsworks_cookbook_demo.tar.gz@ .
sURL :: Lens' Source (Maybe Text)
sURL = lens _sURL (\s a -> s {_sURL = a})

-- | This parameter depends on the repository type.     * For Amazon S3 bundles, set @Username@ to the appropriate IAM access key ID.     * For HTTP bundles, Git repositories, and Subversion repositories, set @Username@ to the user name.
sUsername :: Lens' Source (Maybe Text)
sUsername = lens _sUsername (\s a -> s {_sUsername = a})

-- | In requests, the repository's SSH key. In responses, AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value.
sSSHKey :: Lens' Source (Maybe Text)
sSSHKey = lens _sSSHKey (\s a -> s {_sSSHKey = a})

-- | When included in a request, the parameter depends on the repository type.     * For Amazon S3 bundles, set @Password@ to the appropriate IAM secret access key.     * For HTTP bundles and Subversion repositories, set @Password@ to the password. For more information on how to safely handle IAM credentials, see <https://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html https://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html> . In responses, AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value.
sPassword :: Lens' Source (Maybe Text)
sPassword = lens _sPassword (\s a -> s {_sPassword = a})

-- | The repository type.
sType :: Lens' Source (Maybe SourceType)
sType = lens _sType (\s a -> s {_sType = a})

-- | The application's version. AWS OpsWorks Stacks enables you to easily deploy new versions of an application. One of the simplest approaches is to have branches or revisions in your repository that represent different versions that can potentially be deployed.
sRevision :: Lens' Source (Maybe Text)
sRevision = lens _sRevision (\s a -> s {_sRevision = a})

instance FromJSON Source where
  parseJSON =
    withObject
      "Source"
      ( \x ->
          Source'
            <$> (x .:? "Url")
            <*> (x .:? "Username")
            <*> (x .:? "SshKey")
            <*> (x .:? "Password")
            <*> (x .:? "Type")
            <*> (x .:? "Revision")
      )

instance Hashable Source

instance NFData Source

instance ToJSON Source where
  toJSON Source' {..} =
    object
      ( catMaybes
          [ ("Url" .=) <$> _sURL,
            ("Username" .=) <$> _sUsername,
            ("SshKey" .=) <$> _sSSHKey,
            ("Password" .=) <$> _sPassword,
            ("Type" .=) <$> _sType,
            ("Revision" .=) <$> _sRevision
          ]
      )
