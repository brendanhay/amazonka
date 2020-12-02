{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceBundle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceBundle where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkSpaces.Types.ComputeType
import Network.AWS.WorkSpaces.Types.RootStorage
import Network.AWS.WorkSpaces.Types.UserStorage

-- | Describes a WorkSpace bundle.
--
--
--
-- /See:/ 'workspaceBundle' smart constructor.
data WorkspaceBundle = WorkspaceBundle'
  { _wbLastUpdatedTime ::
      !(Maybe POSIX),
    _wbBundleId :: !(Maybe Text),
    _wbOwner :: !(Maybe Text),
    _wbRootStorage :: !(Maybe RootStorage),
    _wbName :: !(Maybe Text),
    _wbImageId :: !(Maybe Text),
    _wbComputeType :: !(Maybe ComputeType),
    _wbUserStorage :: !(Maybe UserStorage),
    _wbDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkspaceBundle' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wbLastUpdatedTime' - The last time that the bundle was updated.
--
-- * 'wbBundleId' - The bundle identifier.
--
-- * 'wbOwner' - The owner of the bundle. This is the account identifier of the owner, or @AMAZON@ if the bundle is provided by AWS.
--
-- * 'wbRootStorage' - The size of the root volume.
--
-- * 'wbName' - The name of the bundle.
--
-- * 'wbImageId' - The image identifier of the bundle.
--
-- * 'wbComputeType' - The compute type. For more information, see <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles> .
--
-- * 'wbUserStorage' - The size of the user storage.
--
-- * 'wbDescription' - A description.
workspaceBundle ::
  WorkspaceBundle
workspaceBundle =
  WorkspaceBundle'
    { _wbLastUpdatedTime = Nothing,
      _wbBundleId = Nothing,
      _wbOwner = Nothing,
      _wbRootStorage = Nothing,
      _wbName = Nothing,
      _wbImageId = Nothing,
      _wbComputeType = Nothing,
      _wbUserStorage = Nothing,
      _wbDescription = Nothing
    }

-- | The last time that the bundle was updated.
wbLastUpdatedTime :: Lens' WorkspaceBundle (Maybe UTCTime)
wbLastUpdatedTime = lens _wbLastUpdatedTime (\s a -> s {_wbLastUpdatedTime = a}) . mapping _Time

-- | The bundle identifier.
wbBundleId :: Lens' WorkspaceBundle (Maybe Text)
wbBundleId = lens _wbBundleId (\s a -> s {_wbBundleId = a})

-- | The owner of the bundle. This is the account identifier of the owner, or @AMAZON@ if the bundle is provided by AWS.
wbOwner :: Lens' WorkspaceBundle (Maybe Text)
wbOwner = lens _wbOwner (\s a -> s {_wbOwner = a})

-- | The size of the root volume.
wbRootStorage :: Lens' WorkspaceBundle (Maybe RootStorage)
wbRootStorage = lens _wbRootStorage (\s a -> s {_wbRootStorage = a})

-- | The name of the bundle.
wbName :: Lens' WorkspaceBundle (Maybe Text)
wbName = lens _wbName (\s a -> s {_wbName = a})

-- | The image identifier of the bundle.
wbImageId :: Lens' WorkspaceBundle (Maybe Text)
wbImageId = lens _wbImageId (\s a -> s {_wbImageId = a})

-- | The compute type. For more information, see <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles> .
wbComputeType :: Lens' WorkspaceBundle (Maybe ComputeType)
wbComputeType = lens _wbComputeType (\s a -> s {_wbComputeType = a})

-- | The size of the user storage.
wbUserStorage :: Lens' WorkspaceBundle (Maybe UserStorage)
wbUserStorage = lens _wbUserStorage (\s a -> s {_wbUserStorage = a})

-- | A description.
wbDescription :: Lens' WorkspaceBundle (Maybe Text)
wbDescription = lens _wbDescription (\s a -> s {_wbDescription = a})

instance FromJSON WorkspaceBundle where
  parseJSON =
    withObject
      "WorkspaceBundle"
      ( \x ->
          WorkspaceBundle'
            <$> (x .:? "LastUpdatedTime")
            <*> (x .:? "BundleId")
            <*> (x .:? "Owner")
            <*> (x .:? "RootStorage")
            <*> (x .:? "Name")
            <*> (x .:? "ImageId")
            <*> (x .:? "ComputeType")
            <*> (x .:? "UserStorage")
            <*> (x .:? "Description")
      )

instance Hashable WorkspaceBundle

instance NFData WorkspaceBundle
