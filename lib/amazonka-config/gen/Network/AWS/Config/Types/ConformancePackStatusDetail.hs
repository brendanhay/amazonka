{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackStatusDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackStatusDetail where

import Network.AWS.Config.Types.ConformancePackState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Status details of a conformance pack.
--
--
--
-- /See:/ 'conformancePackStatusDetail' smart constructor.
data ConformancePackStatusDetail = ConformancePackStatusDetail'
  { _cpsdConformancePackStatusReason ::
      !(Maybe Text),
    _cpsdLastUpdateCompletedTime ::
      !(Maybe POSIX),
    _cpsdConformancePackName :: !Text,
    _cpsdConformancePackId :: !Text,
    _cpsdConformancePackARN :: !Text,
    _cpsdConformancePackState ::
      !ConformancePackState,
    _cpsdStackARN :: !Text,
    _cpsdLastUpdateRequestedTime ::
      !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConformancePackStatusDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpsdConformancePackStatusReason' - The reason of conformance pack creation failure.
--
-- * 'cpsdLastUpdateCompletedTime' - Last time when conformation pack creation and update was successful.
--
-- * 'cpsdConformancePackName' - Name of the conformance pack.
--
-- * 'cpsdConformancePackId' - ID of the conformance pack.
--
-- * 'cpsdConformancePackARN' - Amazon Resource Name (ARN) of comformance pack.
--
-- * 'cpsdConformancePackState' - Indicates deployment status of conformance pack. AWS Config sets the state of the conformance pack to:     * CREATE_IN_PROGRESS when a conformance pack creation is in progress for an account.     * CREATE_COMPLETE when a conformance pack has been successfully created in your account.     * CREATE_FAILED when a conformance pack creation failed in your account.     * DELETE_IN_PROGRESS when a conformance pack deletion is in progress.      * DELETE_FAILED when a conformance pack deletion failed in your account.
--
-- * 'cpsdStackARN' - Amazon Resource Name (ARN) of AWS CloudFormation stack.
--
-- * 'cpsdLastUpdateRequestedTime' - Last time when conformation pack creation and update was requested.
conformancePackStatusDetail ::
  -- | 'cpsdConformancePackName'
  Text ->
  -- | 'cpsdConformancePackId'
  Text ->
  -- | 'cpsdConformancePackARN'
  Text ->
  -- | 'cpsdConformancePackState'
  ConformancePackState ->
  -- | 'cpsdStackARN'
  Text ->
  -- | 'cpsdLastUpdateRequestedTime'
  UTCTime ->
  ConformancePackStatusDetail
conformancePackStatusDetail
  pConformancePackName_
  pConformancePackId_
  pConformancePackARN_
  pConformancePackState_
  pStackARN_
  pLastUpdateRequestedTime_ =
    ConformancePackStatusDetail'
      { _cpsdConformancePackStatusReason =
          Nothing,
        _cpsdLastUpdateCompletedTime = Nothing,
        _cpsdConformancePackName = pConformancePackName_,
        _cpsdConformancePackId = pConformancePackId_,
        _cpsdConformancePackARN = pConformancePackARN_,
        _cpsdConformancePackState = pConformancePackState_,
        _cpsdStackARN = pStackARN_,
        _cpsdLastUpdateRequestedTime = _Time # pLastUpdateRequestedTime_
      }

-- | The reason of conformance pack creation failure.
cpsdConformancePackStatusReason :: Lens' ConformancePackStatusDetail (Maybe Text)
cpsdConformancePackStatusReason = lens _cpsdConformancePackStatusReason (\s a -> s {_cpsdConformancePackStatusReason = a})

-- | Last time when conformation pack creation and update was successful.
cpsdLastUpdateCompletedTime :: Lens' ConformancePackStatusDetail (Maybe UTCTime)
cpsdLastUpdateCompletedTime = lens _cpsdLastUpdateCompletedTime (\s a -> s {_cpsdLastUpdateCompletedTime = a}) . mapping _Time

-- | Name of the conformance pack.
cpsdConformancePackName :: Lens' ConformancePackStatusDetail Text
cpsdConformancePackName = lens _cpsdConformancePackName (\s a -> s {_cpsdConformancePackName = a})

-- | ID of the conformance pack.
cpsdConformancePackId :: Lens' ConformancePackStatusDetail Text
cpsdConformancePackId = lens _cpsdConformancePackId (\s a -> s {_cpsdConformancePackId = a})

-- | Amazon Resource Name (ARN) of comformance pack.
cpsdConformancePackARN :: Lens' ConformancePackStatusDetail Text
cpsdConformancePackARN = lens _cpsdConformancePackARN (\s a -> s {_cpsdConformancePackARN = a})

-- | Indicates deployment status of conformance pack. AWS Config sets the state of the conformance pack to:     * CREATE_IN_PROGRESS when a conformance pack creation is in progress for an account.     * CREATE_COMPLETE when a conformance pack has been successfully created in your account.     * CREATE_FAILED when a conformance pack creation failed in your account.     * DELETE_IN_PROGRESS when a conformance pack deletion is in progress.      * DELETE_FAILED when a conformance pack deletion failed in your account.
cpsdConformancePackState :: Lens' ConformancePackStatusDetail ConformancePackState
cpsdConformancePackState = lens _cpsdConformancePackState (\s a -> s {_cpsdConformancePackState = a})

-- | Amazon Resource Name (ARN) of AWS CloudFormation stack.
cpsdStackARN :: Lens' ConformancePackStatusDetail Text
cpsdStackARN = lens _cpsdStackARN (\s a -> s {_cpsdStackARN = a})

-- | Last time when conformation pack creation and update was requested.
cpsdLastUpdateRequestedTime :: Lens' ConformancePackStatusDetail UTCTime
cpsdLastUpdateRequestedTime = lens _cpsdLastUpdateRequestedTime (\s a -> s {_cpsdLastUpdateRequestedTime = a}) . _Time

instance FromJSON ConformancePackStatusDetail where
  parseJSON =
    withObject
      "ConformancePackStatusDetail"
      ( \x ->
          ConformancePackStatusDetail'
            <$> (x .:? "ConformancePackStatusReason")
            <*> (x .:? "LastUpdateCompletedTime")
            <*> (x .: "ConformancePackName")
            <*> (x .: "ConformancePackId")
            <*> (x .: "ConformancePackArn")
            <*> (x .: "ConformancePackState")
            <*> (x .: "StackArn")
            <*> (x .: "LastUpdateRequestedTime")
      )

instance Hashable ConformancePackStatusDetail

instance NFData ConformancePackStatusDetail
