{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.Types.Resource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Mobile.Types.Resource where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an instance of an AWS resource associated with a project.
--
--
--
-- /See:/ 'resource' smart constructor.
data Resource = Resource'
  { _rFeature :: !(Maybe Text),
    _rArn :: !(Maybe Text),
    _rName :: !(Maybe Text),
    _rAttributes :: !(Maybe (Map Text (Text))),
    _rType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Resource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rFeature' - Undocumented member.
--
-- * 'rArn' - Undocumented member.
--
-- * 'rName' - Undocumented member.
--
-- * 'rAttributes' - Undocumented member.
--
-- * 'rType' - Undocumented member.
resource ::
  Resource
resource =
  Resource'
    { _rFeature = Nothing,
      _rArn = Nothing,
      _rName = Nothing,
      _rAttributes = Nothing,
      _rType = Nothing
    }

-- | Undocumented member.
rFeature :: Lens' Resource (Maybe Text)
rFeature = lens _rFeature (\s a -> s {_rFeature = a})

-- | Undocumented member.
rArn :: Lens' Resource (Maybe Text)
rArn = lens _rArn (\s a -> s {_rArn = a})

-- | Undocumented member.
rName :: Lens' Resource (Maybe Text)
rName = lens _rName (\s a -> s {_rName = a})

-- | Undocumented member.
rAttributes :: Lens' Resource (HashMap Text (Text))
rAttributes = lens _rAttributes (\s a -> s {_rAttributes = a}) . _Default . _Map

-- | Undocumented member.
rType :: Lens' Resource (Maybe Text)
rType = lens _rType (\s a -> s {_rType = a})

instance FromJSON Resource where
  parseJSON =
    withObject
      "Resource"
      ( \x ->
          Resource'
            <$> (x .:? "feature")
            <*> (x .:? "arn")
            <*> (x .:? "name")
            <*> (x .:? "attributes" .!= mempty)
            <*> (x .:? "type")
      )

instance Hashable Resource

instance NFData Resource
