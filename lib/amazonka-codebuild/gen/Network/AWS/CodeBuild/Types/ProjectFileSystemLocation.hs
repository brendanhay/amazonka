{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ProjectFileSystemLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ProjectFileSystemLocation where

import Network.AWS.CodeBuild.Types.FileSystemType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a file system created by Amazon Elastic File System (EFS). For more information, see <https://docs.aws.amazon.com/efs/latest/ug/whatisefs.html What Is Amazon Elastic File System?>
--
--
--
-- /See:/ 'projectFileSystemLocation' smart constructor.
data ProjectFileSystemLocation = ProjectFileSystemLocation'
  { _pfslLocation ::
      !(Maybe Text),
    _pfslIdentifier :: !(Maybe Text),
    _pfslMountOptions :: !(Maybe Text),
    _pfslType :: !(Maybe FileSystemType),
    _pfslMountPoint :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProjectFileSystemLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pfslLocation' - A string that specifies the location of the file system created by Amazon EFS. Its format is @efs-dns-name:/directory-path@ . You can find the DNS name of file system when you view it in the AWS EFS console. The directory path is a path to a directory in the file system that CodeBuild mounts. For example, if the DNS name of a file system is @fs-abcd1234.efs.us-west-2.amazonaws.com@ , and its mount directory is @my-efs-mount-directory@ , then the @location@ is @fs-abcd1234.efs.us-west-2.amazonaws.com:/my-efs-mount-directory@ .  The directory path in the format @efs-dns-name:/directory-path@ is optional. If you do not specify a directory path, the location is only the DNS name and CodeBuild mounts the entire file system.
--
-- * 'pfslIdentifier' - The name used to access a file system created by Amazon EFS. CodeBuild creates an environment variable by appending the @identifier@ in all capital letters to @CODEBUILD_@ . For example, if you specify @my_efs@ for @identifier@ , a new environment variable is create named @CODEBUILD_MY_EFS@ .  The @identifier@ is used to mount your file system.
--
-- * 'pfslMountOptions' - The mount options for a file system created by AWS EFS. The default mount options used by CodeBuild are @nfsvers=4.1,rsize=1048576,wsize=1048576,hard,timeo=600,retrans=2@ . For more information, see <https://docs.aws.amazon.com/efs/latest/ug/mounting-fs-nfs-mount-settings.html Recommended NFS Mount Options> .
--
-- * 'pfslType' - The type of the file system. The one supported type is @EFS@ .
--
-- * 'pfslMountPoint' - The location in the container where you mount the file system.
projectFileSystemLocation ::
  ProjectFileSystemLocation
projectFileSystemLocation =
  ProjectFileSystemLocation'
    { _pfslLocation = Nothing,
      _pfslIdentifier = Nothing,
      _pfslMountOptions = Nothing,
      _pfslType = Nothing,
      _pfslMountPoint = Nothing
    }

-- | A string that specifies the location of the file system created by Amazon EFS. Its format is @efs-dns-name:/directory-path@ . You can find the DNS name of file system when you view it in the AWS EFS console. The directory path is a path to a directory in the file system that CodeBuild mounts. For example, if the DNS name of a file system is @fs-abcd1234.efs.us-west-2.amazonaws.com@ , and its mount directory is @my-efs-mount-directory@ , then the @location@ is @fs-abcd1234.efs.us-west-2.amazonaws.com:/my-efs-mount-directory@ .  The directory path in the format @efs-dns-name:/directory-path@ is optional. If you do not specify a directory path, the location is only the DNS name and CodeBuild mounts the entire file system.
pfslLocation :: Lens' ProjectFileSystemLocation (Maybe Text)
pfslLocation = lens _pfslLocation (\s a -> s {_pfslLocation = a})

-- | The name used to access a file system created by Amazon EFS. CodeBuild creates an environment variable by appending the @identifier@ in all capital letters to @CODEBUILD_@ . For example, if you specify @my_efs@ for @identifier@ , a new environment variable is create named @CODEBUILD_MY_EFS@ .  The @identifier@ is used to mount your file system.
pfslIdentifier :: Lens' ProjectFileSystemLocation (Maybe Text)
pfslIdentifier = lens _pfslIdentifier (\s a -> s {_pfslIdentifier = a})

-- | The mount options for a file system created by AWS EFS. The default mount options used by CodeBuild are @nfsvers=4.1,rsize=1048576,wsize=1048576,hard,timeo=600,retrans=2@ . For more information, see <https://docs.aws.amazon.com/efs/latest/ug/mounting-fs-nfs-mount-settings.html Recommended NFS Mount Options> .
pfslMountOptions :: Lens' ProjectFileSystemLocation (Maybe Text)
pfslMountOptions = lens _pfslMountOptions (\s a -> s {_pfslMountOptions = a})

-- | The type of the file system. The one supported type is @EFS@ .
pfslType :: Lens' ProjectFileSystemLocation (Maybe FileSystemType)
pfslType = lens _pfslType (\s a -> s {_pfslType = a})

-- | The location in the container where you mount the file system.
pfslMountPoint :: Lens' ProjectFileSystemLocation (Maybe Text)
pfslMountPoint = lens _pfslMountPoint (\s a -> s {_pfslMountPoint = a})

instance FromJSON ProjectFileSystemLocation where
  parseJSON =
    withObject
      "ProjectFileSystemLocation"
      ( \x ->
          ProjectFileSystemLocation'
            <$> (x .:? "location")
            <*> (x .:? "identifier")
            <*> (x .:? "mountOptions")
            <*> (x .:? "type")
            <*> (x .:? "mountPoint")
      )

instance Hashable ProjectFileSystemLocation

instance NFData ProjectFileSystemLocation

instance ToJSON ProjectFileSystemLocation where
  toJSON ProjectFileSystemLocation' {..} =
    object
      ( catMaybes
          [ ("location" .=) <$> _pfslLocation,
            ("identifier" .=) <$> _pfslIdentifier,
            ("mountOptions" .=) <$> _pfslMountOptions,
            ("type" .=) <$> _pfslType,
            ("mountPoint" .=) <$> _pfslMountPoint
          ]
      )
