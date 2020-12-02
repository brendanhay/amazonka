{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.DescribeImagesFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.DescribeImagesFilter where

import Network.AWS.ECR.Types.TagStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing a filter on a 'DescribeImages' operation.
--
--
--
-- /See:/ 'describeImagesFilter' smart constructor.
newtype DescribeImagesFilter = DescribeImagesFilter'
  { _difTagStatus ::
      Maybe TagStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeImagesFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'difTagStatus' - The tag status with which to filter your 'DescribeImages' results. You can filter results based on whether they are @TAGGED@ or @UNTAGGED@ .
describeImagesFilter ::
  DescribeImagesFilter
describeImagesFilter =
  DescribeImagesFilter' {_difTagStatus = Nothing}

-- | The tag status with which to filter your 'DescribeImages' results. You can filter results based on whether they are @TAGGED@ or @UNTAGGED@ .
difTagStatus :: Lens' DescribeImagesFilter (Maybe TagStatus)
difTagStatus = lens _difTagStatus (\s a -> s {_difTagStatus = a})

instance Hashable DescribeImagesFilter

instance NFData DescribeImagesFilter

instance ToJSON DescribeImagesFilter where
  toJSON DescribeImagesFilter' {..} =
    object (catMaybes [("tagStatus" .=) <$> _difTagStatus])
