{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.SkillGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SkillGroup where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A skill group with attributes.
--
--
--
-- /See:/ 'skillGroup' smart constructor.
data SkillGroup = SkillGroup'
  { _sgSkillGroupARN :: !(Maybe Text),
    _sgDescription :: !(Maybe Text),
    _sgSkillGroupName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SkillGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgSkillGroupARN' - The ARN of a skill group.
--
-- * 'sgDescription' - The description of a skill group.
--
-- * 'sgSkillGroupName' - The name of a skill group.
skillGroup ::
  SkillGroup
skillGroup =
  SkillGroup'
    { _sgSkillGroupARN = Nothing,
      _sgDescription = Nothing,
      _sgSkillGroupName = Nothing
    }

-- | The ARN of a skill group.
sgSkillGroupARN :: Lens' SkillGroup (Maybe Text)
sgSkillGroupARN = lens _sgSkillGroupARN (\s a -> s {_sgSkillGroupARN = a})

-- | The description of a skill group.
sgDescription :: Lens' SkillGroup (Maybe Text)
sgDescription = lens _sgDescription (\s a -> s {_sgDescription = a})

-- | The name of a skill group.
sgSkillGroupName :: Lens' SkillGroup (Maybe Text)
sgSkillGroupName = lens _sgSkillGroupName (\s a -> s {_sgSkillGroupName = a})

instance FromJSON SkillGroup where
  parseJSON =
    withObject
      "SkillGroup"
      ( \x ->
          SkillGroup'
            <$> (x .:? "SkillGroupArn")
            <*> (x .:? "Description")
            <*> (x .:? "SkillGroupName")
      )

instance Hashable SkillGroup

instance NFData SkillGroup
