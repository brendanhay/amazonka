{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.RevisionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.RevisionInfo where

import Network.AWS.CodeDeploy.Types.GenericRevisionInfo
import Network.AWS.CodeDeploy.Types.RevisionLocation
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an application revision.
--
--
--
-- /See:/ 'revisionInfo' smart constructor.
data RevisionInfo = RevisionInfo'
  { _riGenericRevisionInfo ::
      !(Maybe GenericRevisionInfo),
    _riRevisionLocation :: !(Maybe RevisionLocation)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RevisionInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riGenericRevisionInfo' - Information about an application revision, including usage details and associated deployment groups.
--
-- * 'riRevisionLocation' - Information about the location and type of an application revision.
revisionInfo ::
  RevisionInfo
revisionInfo =
  RevisionInfo'
    { _riGenericRevisionInfo = Nothing,
      _riRevisionLocation = Nothing
    }

-- | Information about an application revision, including usage details and associated deployment groups.
riGenericRevisionInfo :: Lens' RevisionInfo (Maybe GenericRevisionInfo)
riGenericRevisionInfo = lens _riGenericRevisionInfo (\s a -> s {_riGenericRevisionInfo = a})

-- | Information about the location and type of an application revision.
riRevisionLocation :: Lens' RevisionInfo (Maybe RevisionLocation)
riRevisionLocation = lens _riRevisionLocation (\s a -> s {_riRevisionLocation = a})

instance FromJSON RevisionInfo where
  parseJSON =
    withObject
      "RevisionInfo"
      ( \x ->
          RevisionInfo'
            <$> (x .:? "genericRevisionInfo") <*> (x .:? "revisionLocation")
      )

instance Hashable RevisionInfo

instance NFData RevisionInfo
