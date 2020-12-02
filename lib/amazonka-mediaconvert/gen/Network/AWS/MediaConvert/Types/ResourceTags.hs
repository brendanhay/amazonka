{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ResourceTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ResourceTags where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Amazon Resource Name (ARN) and tags for an AWS Elemental MediaConvert resource.
--
-- /See:/ 'resourceTags' smart constructor.
data ResourceTags = ResourceTags'
  { _rtARN :: !(Maybe Text),
    _rtTags :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtARN' - The Amazon Resource Name (ARN) of the resource.
--
-- * 'rtTags' - The tags for the resource.
resourceTags ::
  ResourceTags
resourceTags = ResourceTags' {_rtARN = Nothing, _rtTags = Nothing}

-- | The Amazon Resource Name (ARN) of the resource.
rtARN :: Lens' ResourceTags (Maybe Text)
rtARN = lens _rtARN (\s a -> s {_rtARN = a})

-- | The tags for the resource.
rtTags :: Lens' ResourceTags (HashMap Text (Text))
rtTags = lens _rtTags (\s a -> s {_rtTags = a}) . _Default . _Map

instance FromJSON ResourceTags where
  parseJSON =
    withObject
      "ResourceTags"
      ( \x ->
          ResourceTags' <$> (x .:? "arn") <*> (x .:? "tags" .!= mempty)
      )

instance Hashable ResourceTags

instance NFData ResourceTags
