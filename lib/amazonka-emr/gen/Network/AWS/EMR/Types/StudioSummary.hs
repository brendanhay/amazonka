{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.StudioSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StudioSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details for an Amazon EMR Studio, including ID, Name, VPC, and Description. The details do not include subnets, IAM roles, security groups, or tags associated with the Studio.
--
--
--
-- /See:/ 'studioSummary' smart constructor.
data StudioSummary = StudioSummary'
  { _ssCreationTime ::
      !(Maybe POSIX),
    _ssStudioId :: !(Maybe Text),
    _ssVPCId :: !(Maybe Text),
    _ssURL :: !(Maybe Text),
    _ssName :: !(Maybe Text),
    _ssDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StudioSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssCreationTime' - The time when the Amazon EMR Studio was created.
--
-- * 'ssStudioId' - The ID of the Amazon EMR Studio.
--
-- * 'ssVPCId' - The ID of the Virtual Private Cloud (Amazon VPC) associated with the Amazon EMR Studio.
--
-- * 'ssURL' - The unique access URL of the Amazon EMR Studio.
--
-- * 'ssName' - The name of the Amazon EMR Studio.
--
-- * 'ssDescription' - The detailed description of the EMR Studio.
studioSummary ::
  StudioSummary
studioSummary =
  StudioSummary'
    { _ssCreationTime = Nothing,
      _ssStudioId = Nothing,
      _ssVPCId = Nothing,
      _ssURL = Nothing,
      _ssName = Nothing,
      _ssDescription = Nothing
    }

-- | The time when the Amazon EMR Studio was created.
ssCreationTime :: Lens' StudioSummary (Maybe UTCTime)
ssCreationTime = lens _ssCreationTime (\s a -> s {_ssCreationTime = a}) . mapping _Time

-- | The ID of the Amazon EMR Studio.
ssStudioId :: Lens' StudioSummary (Maybe Text)
ssStudioId = lens _ssStudioId (\s a -> s {_ssStudioId = a})

-- | The ID of the Virtual Private Cloud (Amazon VPC) associated with the Amazon EMR Studio.
ssVPCId :: Lens' StudioSummary (Maybe Text)
ssVPCId = lens _ssVPCId (\s a -> s {_ssVPCId = a})

-- | The unique access URL of the Amazon EMR Studio.
ssURL :: Lens' StudioSummary (Maybe Text)
ssURL = lens _ssURL (\s a -> s {_ssURL = a})

-- | The name of the Amazon EMR Studio.
ssName :: Lens' StudioSummary (Maybe Text)
ssName = lens _ssName (\s a -> s {_ssName = a})

-- | The detailed description of the EMR Studio.
ssDescription :: Lens' StudioSummary (Maybe Text)
ssDescription = lens _ssDescription (\s a -> s {_ssDescription = a})

instance FromJSON StudioSummary where
  parseJSON =
    withObject
      "StudioSummary"
      ( \x ->
          StudioSummary'
            <$> (x .:? "CreationTime")
            <*> (x .:? "StudioId")
            <*> (x .:? "VpcId")
            <*> (x .:? "Url")
            <*> (x .:? "Name")
            <*> (x .:? "Description")
      )

instance Hashable StudioSummary

instance NFData StudioSummary
