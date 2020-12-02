{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageFailure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageFailure where

import Network.AWS.ECR.Types.ImageFailureCode
import Network.AWS.ECR.Types.ImageIdentifier
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing an Amazon ECR image failure.
--
--
--
-- /See:/ 'imageFailure' smart constructor.
data ImageFailure = ImageFailure'
  { _ifFailureReason ::
      !(Maybe Text),
    _ifFailureCode :: !(Maybe ImageFailureCode),
    _ifImageId :: !(Maybe ImageIdentifier)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImageFailure' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifFailureReason' - The reason for the failure.
--
-- * 'ifFailureCode' - The code associated with the failure.
--
-- * 'ifImageId' - The image ID associated with the failure.
imageFailure ::
  ImageFailure
imageFailure =
  ImageFailure'
    { _ifFailureReason = Nothing,
      _ifFailureCode = Nothing,
      _ifImageId = Nothing
    }

-- | The reason for the failure.
ifFailureReason :: Lens' ImageFailure (Maybe Text)
ifFailureReason = lens _ifFailureReason (\s a -> s {_ifFailureReason = a})

-- | The code associated with the failure.
ifFailureCode :: Lens' ImageFailure (Maybe ImageFailureCode)
ifFailureCode = lens _ifFailureCode (\s a -> s {_ifFailureCode = a})

-- | The image ID associated with the failure.
ifImageId :: Lens' ImageFailure (Maybe ImageIdentifier)
ifImageId = lens _ifImageId (\s a -> s {_ifImageId = a})

instance FromJSON ImageFailure where
  parseJSON =
    withObject
      "ImageFailure"
      ( \x ->
          ImageFailure'
            <$> (x .:? "failureReason")
            <*> (x .:? "failureCode")
            <*> (x .:? "imageId")
      )

instance Hashable ImageFailure

instance NFData ImageFailure
