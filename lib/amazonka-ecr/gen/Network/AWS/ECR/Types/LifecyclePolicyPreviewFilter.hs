{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.LifecyclePolicyPreviewFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.LifecyclePolicyPreviewFilter where

import Network.AWS.ECR.Types.TagStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The filter for the lifecycle policy preview.
--
--
--
-- /See:/ 'lifecyclePolicyPreviewFilter' smart constructor.
newtype LifecyclePolicyPreviewFilter = LifecyclePolicyPreviewFilter'
  { _lppfTagStatus ::
      Maybe TagStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LifecyclePolicyPreviewFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lppfTagStatus' - The tag status of the image.
lifecyclePolicyPreviewFilter ::
  LifecyclePolicyPreviewFilter
lifecyclePolicyPreviewFilter =
  LifecyclePolicyPreviewFilter' {_lppfTagStatus = Nothing}

-- | The tag status of the image.
lppfTagStatus :: Lens' LifecyclePolicyPreviewFilter (Maybe TagStatus)
lppfTagStatus = lens _lppfTagStatus (\s a -> s {_lppfTagStatus = a})

instance Hashable LifecyclePolicyPreviewFilter

instance NFData LifecyclePolicyPreviewFilter

instance ToJSON LifecyclePolicyPreviewFilter where
  toJSON LifecyclePolicyPreviewFilter' {..} =
    object (catMaybes [("tagStatus" .=) <$> _lppfTagStatus])
