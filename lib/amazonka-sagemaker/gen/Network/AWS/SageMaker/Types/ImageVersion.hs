{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ImageVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ImageVersion where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ImageVersionStatus

-- | A version of a SageMaker @Image@ . A version represents an existing container image.
--
--
--
-- /See:/ 'imageVersion' smart constructor.
data ImageVersion = ImageVersion'
  { _ivFailureReason ::
      !(Maybe Text),
    _ivCreationTime :: !POSIX,
    _ivImageARN :: !Text,
    _ivImageVersionARN :: !Text,
    _ivImageVersionStatus :: !ImageVersionStatus,
    _ivLastModifiedTime :: !POSIX,
    _ivVersion :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImageVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ivFailureReason' - When a create or delete operation fails, the reason for the failure.
--
-- * 'ivCreationTime' - When the version was created.
--
-- * 'ivImageARN' - The Amazon Resource Name (ARN) of the image the version is based on.
--
-- * 'ivImageVersionARN' - The ARN of the version.
--
-- * 'ivImageVersionStatus' - The status of the version.
--
-- * 'ivLastModifiedTime' - When the version was last modified.
--
-- * 'ivVersion' - The version number.
imageVersion ::
  -- | 'ivCreationTime'
  UTCTime ->
  -- | 'ivImageARN'
  Text ->
  -- | 'ivImageVersionARN'
  Text ->
  -- | 'ivImageVersionStatus'
  ImageVersionStatus ->
  -- | 'ivLastModifiedTime'
  UTCTime ->
  -- | 'ivVersion'
  Natural ->
  ImageVersion
imageVersion
  pCreationTime_
  pImageARN_
  pImageVersionARN_
  pImageVersionStatus_
  pLastModifiedTime_
  pVersion_ =
    ImageVersion'
      { _ivFailureReason = Nothing,
        _ivCreationTime = _Time # pCreationTime_,
        _ivImageARN = pImageARN_,
        _ivImageVersionARN = pImageVersionARN_,
        _ivImageVersionStatus = pImageVersionStatus_,
        _ivLastModifiedTime = _Time # pLastModifiedTime_,
        _ivVersion = _Nat # pVersion_
      }

-- | When a create or delete operation fails, the reason for the failure.
ivFailureReason :: Lens' ImageVersion (Maybe Text)
ivFailureReason = lens _ivFailureReason (\s a -> s {_ivFailureReason = a})

-- | When the version was created.
ivCreationTime :: Lens' ImageVersion UTCTime
ivCreationTime = lens _ivCreationTime (\s a -> s {_ivCreationTime = a}) . _Time

-- | The Amazon Resource Name (ARN) of the image the version is based on.
ivImageARN :: Lens' ImageVersion Text
ivImageARN = lens _ivImageARN (\s a -> s {_ivImageARN = a})

-- | The ARN of the version.
ivImageVersionARN :: Lens' ImageVersion Text
ivImageVersionARN = lens _ivImageVersionARN (\s a -> s {_ivImageVersionARN = a})

-- | The status of the version.
ivImageVersionStatus :: Lens' ImageVersion ImageVersionStatus
ivImageVersionStatus = lens _ivImageVersionStatus (\s a -> s {_ivImageVersionStatus = a})

-- | When the version was last modified.
ivLastModifiedTime :: Lens' ImageVersion UTCTime
ivLastModifiedTime = lens _ivLastModifiedTime (\s a -> s {_ivLastModifiedTime = a}) . _Time

-- | The version number.
ivVersion :: Lens' ImageVersion Natural
ivVersion = lens _ivVersion (\s a -> s {_ivVersion = a}) . _Nat

instance FromJSON ImageVersion where
  parseJSON =
    withObject
      "ImageVersion"
      ( \x ->
          ImageVersion'
            <$> (x .:? "FailureReason")
            <*> (x .: "CreationTime")
            <*> (x .: "ImageArn")
            <*> (x .: "ImageVersionArn")
            <*> (x .: "ImageVersionStatus")
            <*> (x .: "LastModifiedTime")
            <*> (x .: "Version")
      )

instance Hashable ImageVersion

instance NFData ImageVersion
