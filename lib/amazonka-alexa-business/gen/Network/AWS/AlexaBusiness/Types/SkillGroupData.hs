{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.SkillGroupData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SkillGroupData where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The attributes of a skill group.
--
--
--
-- /See:/ 'skillGroupData' smart constructor.
data SkillGroupData = SkillGroupData'
  { _sgdSkillGroupARN ::
      !(Maybe Text),
    _sgdDescription :: !(Maybe Text),
    _sgdSkillGroupName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SkillGroupData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgdSkillGroupARN' - The skill group ARN of a skill group.
--
-- * 'sgdDescription' - The description of a skill group.
--
-- * 'sgdSkillGroupName' - The skill group name of a skill group.
skillGroupData ::
  SkillGroupData
skillGroupData =
  SkillGroupData'
    { _sgdSkillGroupARN = Nothing,
      _sgdDescription = Nothing,
      _sgdSkillGroupName = Nothing
    }

-- | The skill group ARN of a skill group.
sgdSkillGroupARN :: Lens' SkillGroupData (Maybe Text)
sgdSkillGroupARN = lens _sgdSkillGroupARN (\s a -> s {_sgdSkillGroupARN = a})

-- | The description of a skill group.
sgdDescription :: Lens' SkillGroupData (Maybe Text)
sgdDescription = lens _sgdDescription (\s a -> s {_sgdDescription = a})

-- | The skill group name of a skill group.
sgdSkillGroupName :: Lens' SkillGroupData (Maybe Text)
sgdSkillGroupName = lens _sgdSkillGroupName (\s a -> s {_sgdSkillGroupName = a})

instance FromJSON SkillGroupData where
  parseJSON =
    withObject
      "SkillGroupData"
      ( \x ->
          SkillGroupData'
            <$> (x .:? "SkillGroupArn")
            <*> (x .:? "Description")
            <*> (x .:? "SkillGroupName")
      )

instance Hashable SkillGroupData

instance NFData SkillGroupData
