{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ApplicationSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ApplicationSource where

import Network.AWS.AutoScalingPlans.Types.TagFilter
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an application source.
--
--
--
-- /See:/ 'applicationSource' smart constructor.
data ApplicationSource = ApplicationSource'
  { _asTagFilters ::
      !(Maybe [TagFilter]),
    _asCloudFormationStackARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ApplicationSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asTagFilters' - A set of tags (up to 50).
--
-- * 'asCloudFormationStackARN' - The Amazon Resource Name (ARN) of a AWS CloudFormation stack.
applicationSource ::
  ApplicationSource
applicationSource =
  ApplicationSource'
    { _asTagFilters = Nothing,
      _asCloudFormationStackARN = Nothing
    }

-- | A set of tags (up to 50).
asTagFilters :: Lens' ApplicationSource [TagFilter]
asTagFilters = lens _asTagFilters (\s a -> s {_asTagFilters = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of a AWS CloudFormation stack.
asCloudFormationStackARN :: Lens' ApplicationSource (Maybe Text)
asCloudFormationStackARN = lens _asCloudFormationStackARN (\s a -> s {_asCloudFormationStackARN = a})

instance FromJSON ApplicationSource where
  parseJSON =
    withObject
      "ApplicationSource"
      ( \x ->
          ApplicationSource'
            <$> (x .:? "TagFilters" .!= mempty)
            <*> (x .:? "CloudFormationStackARN")
      )

instance Hashable ApplicationSource

instance NFData ApplicationSource

instance ToJSON ApplicationSource where
  toJSON ApplicationSource' {..} =
    object
      ( catMaybes
          [ ("TagFilters" .=) <$> _asTagFilters,
            ("CloudFormationStackARN" .=) <$> _asCloudFormationStackARN
          ]
      )
