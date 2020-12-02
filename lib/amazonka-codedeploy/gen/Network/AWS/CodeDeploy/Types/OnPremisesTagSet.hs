{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.OnPremisesTagSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.OnPremisesTagSet where

import Network.AWS.CodeDeploy.Types.TagFilter
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about groups of on-premises instance tags.
--
--
--
-- /See:/ 'onPremisesTagSet' smart constructor.
newtype OnPremisesTagSet = OnPremisesTagSet'
  { _optsOnPremisesTagSetList ::
      Maybe [[TagFilter]]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OnPremisesTagSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'optsOnPremisesTagSetList' - A list that contains other lists of on-premises instance tag groups. For an instance to be included in the deployment group, it must be identified by all of the tag groups in the list.
onPremisesTagSet ::
  OnPremisesTagSet
onPremisesTagSet =
  OnPremisesTagSet' {_optsOnPremisesTagSetList = Nothing}

-- | A list that contains other lists of on-premises instance tag groups. For an instance to be included in the deployment group, it must be identified by all of the tag groups in the list.
optsOnPremisesTagSetList :: Lens' OnPremisesTagSet [[TagFilter]]
optsOnPremisesTagSetList = lens _optsOnPremisesTagSetList (\s a -> s {_optsOnPremisesTagSetList = a}) . _Default . _Coerce

instance FromJSON OnPremisesTagSet where
  parseJSON =
    withObject
      "OnPremisesTagSet"
      ( \x ->
          OnPremisesTagSet' <$> (x .:? "onPremisesTagSetList" .!= mempty)
      )

instance Hashable OnPremisesTagSet

instance NFData OnPremisesTagSet

instance ToJSON OnPremisesTagSet where
  toJSON OnPremisesTagSet' {..} =
    object
      ( catMaybes
          [("onPremisesTagSetList" .=) <$> _optsOnPremisesTagSetList]
      )
