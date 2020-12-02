{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.PolicyVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.PolicyVersion where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a policy version.
--
--
--
-- /See:/ 'policyVersion' smart constructor.
data PolicyVersion = PolicyVersion'
  { _pvVersionId :: !(Maybe Text),
    _pvCreateDate :: !(Maybe POSIX),
    _pvIsDefaultVersion :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PolicyVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvVersionId' - The policy version ID.
--
-- * 'pvCreateDate' - The date and time the policy was created.
--
-- * 'pvIsDefaultVersion' - Specifies whether the policy version is the default.
policyVersion ::
  PolicyVersion
policyVersion =
  PolicyVersion'
    { _pvVersionId = Nothing,
      _pvCreateDate = Nothing,
      _pvIsDefaultVersion = Nothing
    }

-- | The policy version ID.
pvVersionId :: Lens' PolicyVersion (Maybe Text)
pvVersionId = lens _pvVersionId (\s a -> s {_pvVersionId = a})

-- | The date and time the policy was created.
pvCreateDate :: Lens' PolicyVersion (Maybe UTCTime)
pvCreateDate = lens _pvCreateDate (\s a -> s {_pvCreateDate = a}) . mapping _Time

-- | Specifies whether the policy version is the default.
pvIsDefaultVersion :: Lens' PolicyVersion (Maybe Bool)
pvIsDefaultVersion = lens _pvIsDefaultVersion (\s a -> s {_pvIsDefaultVersion = a})

instance FromJSON PolicyVersion where
  parseJSON =
    withObject
      "PolicyVersion"
      ( \x ->
          PolicyVersion'
            <$> (x .:? "versionId")
            <*> (x .:? "createDate")
            <*> (x .:? "isDefaultVersion")
      )

instance Hashable PolicyVersion

instance NFData PolicyVersion
