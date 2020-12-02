{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.Builder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.Builder where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The builder used to build the custom platform.
--
--
--
-- /See:/ 'builder' smart constructor.
newtype Builder = Builder' {_bARN :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Builder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bARN' - The ARN of the builder.
builder ::
  Builder
builder = Builder' {_bARN = Nothing}

-- | The ARN of the builder.
bARN :: Lens' Builder (Maybe Text)
bARN = lens _bARN (\s a -> s {_bARN = a})

instance FromXML Builder where
  parseXML x = Builder' <$> (x .@? "ARN")

instance Hashable Builder

instance NFData Builder
