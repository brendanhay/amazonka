{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ListImagesFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ListImagesFilter where

import Network.AWS.ECR.Types.TagStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing a filter on a 'ListImages' operation.
--
--
--
-- /See:/ 'listImagesFilter' smart constructor.
newtype ListImagesFilter = ListImagesFilter'
  { _lifTagStatus ::
      Maybe TagStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListImagesFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lifTagStatus' - The tag status with which to filter your 'ListImages' results. You can filter results based on whether they are @TAGGED@ or @UNTAGGED@ .
listImagesFilter ::
  ListImagesFilter
listImagesFilter = ListImagesFilter' {_lifTagStatus = Nothing}

-- | The tag status with which to filter your 'ListImages' results. You can filter results based on whether they are @TAGGED@ or @UNTAGGED@ .
lifTagStatus :: Lens' ListImagesFilter (Maybe TagStatus)
lifTagStatus = lens _lifTagStatus (\s a -> s {_lifTagStatus = a})

instance Hashable ListImagesFilter

instance NFData ListImagesFilter

instance ToJSON ListImagesFilter where
  toJSON ListImagesFilter' {..} =
    object (catMaybes [("tagStatus" .=) <$> _lifTagStatus])
