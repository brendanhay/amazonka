{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.GenericRevisionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.GenericRevisionInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an application revision.
--
--
--
-- /See:/ 'genericRevisionInfo' smart constructor.
data GenericRevisionInfo = GenericRevisionInfo'
  { _griRegisterTime ::
      !(Maybe POSIX),
    _griFirstUsedTime :: !(Maybe POSIX),
    _griDeploymentGroups :: !(Maybe [Text]),
    _griLastUsedTime :: !(Maybe POSIX),
    _griDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GenericRevisionInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'griRegisterTime' - When the revision was registered with AWS CodeDeploy.
--
-- * 'griFirstUsedTime' - When the revision was first used by AWS CodeDeploy.
--
-- * 'griDeploymentGroups' - The deployment groups for which this is the current target revision.
--
-- * 'griLastUsedTime' - When the revision was last used by AWS CodeDeploy.
--
-- * 'griDescription' - A comment about the revision.
genericRevisionInfo ::
  GenericRevisionInfo
genericRevisionInfo =
  GenericRevisionInfo'
    { _griRegisterTime = Nothing,
      _griFirstUsedTime = Nothing,
      _griDeploymentGroups = Nothing,
      _griLastUsedTime = Nothing,
      _griDescription = Nothing
    }

-- | When the revision was registered with AWS CodeDeploy.
griRegisterTime :: Lens' GenericRevisionInfo (Maybe UTCTime)
griRegisterTime = lens _griRegisterTime (\s a -> s {_griRegisterTime = a}) . mapping _Time

-- | When the revision was first used by AWS CodeDeploy.
griFirstUsedTime :: Lens' GenericRevisionInfo (Maybe UTCTime)
griFirstUsedTime = lens _griFirstUsedTime (\s a -> s {_griFirstUsedTime = a}) . mapping _Time

-- | The deployment groups for which this is the current target revision.
griDeploymentGroups :: Lens' GenericRevisionInfo [Text]
griDeploymentGroups = lens _griDeploymentGroups (\s a -> s {_griDeploymentGroups = a}) . _Default . _Coerce

-- | When the revision was last used by AWS CodeDeploy.
griLastUsedTime :: Lens' GenericRevisionInfo (Maybe UTCTime)
griLastUsedTime = lens _griLastUsedTime (\s a -> s {_griLastUsedTime = a}) . mapping _Time

-- | A comment about the revision.
griDescription :: Lens' GenericRevisionInfo (Maybe Text)
griDescription = lens _griDescription (\s a -> s {_griDescription = a})

instance FromJSON GenericRevisionInfo where
  parseJSON =
    withObject
      "GenericRevisionInfo"
      ( \x ->
          GenericRevisionInfo'
            <$> (x .:? "registerTime")
            <*> (x .:? "firstUsedTime")
            <*> (x .:? "deploymentGroups" .!= mempty)
            <*> (x .:? "lastUsedTime")
            <*> (x .:? "description")
      )

instance Hashable GenericRevisionInfo

instance NFData GenericRevisionInfo
