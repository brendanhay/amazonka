{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.Resource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.Resource where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkMail.Types.EntityState
import Network.AWS.WorkMail.Types.ResourceType

-- | The representation of a resource.
--
--
--
-- /See:/ 'resource' smart constructor.
data Resource = Resource'
  { _rEmail :: !(Maybe Text),
    _rState :: !(Maybe EntityState),
    _rDisabledDate :: !(Maybe POSIX),
    _rName :: !(Maybe Text),
    _rId :: !(Maybe Text),
    _rType :: !(Maybe ResourceType),
    _rEnabledDate :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Resource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rEmail' - The email of the resource.
--
-- * 'rState' - The state of the resource, which can be ENABLED, DISABLED, or DELETED.
--
-- * 'rDisabledDate' - The date indicating when the resource was disabled from Amazon WorkMail use.
--
-- * 'rName' - The name of the resource.
--
-- * 'rId' - The identifier of the resource.
--
-- * 'rType' - The type of the resource: equipment or room.
--
-- * 'rEnabledDate' - The date indicating when the resource was enabled for Amazon WorkMail use.
resource ::
  Resource
resource =
  Resource'
    { _rEmail = Nothing,
      _rState = Nothing,
      _rDisabledDate = Nothing,
      _rName = Nothing,
      _rId = Nothing,
      _rType = Nothing,
      _rEnabledDate = Nothing
    }

-- | The email of the resource.
rEmail :: Lens' Resource (Maybe Text)
rEmail = lens _rEmail (\s a -> s {_rEmail = a})

-- | The state of the resource, which can be ENABLED, DISABLED, or DELETED.
rState :: Lens' Resource (Maybe EntityState)
rState = lens _rState (\s a -> s {_rState = a})

-- | The date indicating when the resource was disabled from Amazon WorkMail use.
rDisabledDate :: Lens' Resource (Maybe UTCTime)
rDisabledDate = lens _rDisabledDate (\s a -> s {_rDisabledDate = a}) . mapping _Time

-- | The name of the resource.
rName :: Lens' Resource (Maybe Text)
rName = lens _rName (\s a -> s {_rName = a})

-- | The identifier of the resource.
rId :: Lens' Resource (Maybe Text)
rId = lens _rId (\s a -> s {_rId = a})

-- | The type of the resource: equipment or room.
rType :: Lens' Resource (Maybe ResourceType)
rType = lens _rType (\s a -> s {_rType = a})

-- | The date indicating when the resource was enabled for Amazon WorkMail use.
rEnabledDate :: Lens' Resource (Maybe UTCTime)
rEnabledDate = lens _rEnabledDate (\s a -> s {_rEnabledDate = a}) . mapping _Time

instance FromJSON Resource where
  parseJSON =
    withObject
      "Resource"
      ( \x ->
          Resource'
            <$> (x .:? "Email")
            <*> (x .:? "State")
            <*> (x .:? "DisabledDate")
            <*> (x .:? "Name")
            <*> (x .:? "Id")
            <*> (x .:? "Type")
            <*> (x .:? "EnabledDate")
      )

instance Hashable Resource

instance NFData Resource
