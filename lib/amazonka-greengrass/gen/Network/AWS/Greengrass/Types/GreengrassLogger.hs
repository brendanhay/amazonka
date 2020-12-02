{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.GreengrassLogger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.GreengrassLogger where

import Network.AWS.Greengrass.Types.LoggerComponent
import Network.AWS.Greengrass.Types.LoggerLevel
import Network.AWS.Greengrass.Types.LoggerType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a logger
--
-- /See:/ 'greengrassLogger' smart constructor.
data GreengrassLogger = GreengrassLogger'
  { _glSpace :: !(Maybe Int),
    _glType :: !LoggerType,
    _glLevel :: !LoggerLevel,
    _glId :: !Text,
    _glComponent :: !LoggerComponent
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GreengrassLogger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glSpace' - The amount of file space, in KB, to use if the local file system is used for logging purposes.
--
-- * 'glType' - The type of log output which will be used.
--
-- * 'glLevel' - The level of the logs.
--
-- * 'glId' - A descriptive or arbitrary ID for the logger. This value must be unique within the logger definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
--
-- * 'glComponent' - The component that will be subject to logging.
greengrassLogger ::
  -- | 'glType'
  LoggerType ->
  -- | 'glLevel'
  LoggerLevel ->
  -- | 'glId'
  Text ->
  -- | 'glComponent'
  LoggerComponent ->
  GreengrassLogger
greengrassLogger pType_ pLevel_ pId_ pComponent_ =
  GreengrassLogger'
    { _glSpace = Nothing,
      _glType = pType_,
      _glLevel = pLevel_,
      _glId = pId_,
      _glComponent = pComponent_
    }

-- | The amount of file space, in KB, to use if the local file system is used for logging purposes.
glSpace :: Lens' GreengrassLogger (Maybe Int)
glSpace = lens _glSpace (\s a -> s {_glSpace = a})

-- | The type of log output which will be used.
glType :: Lens' GreengrassLogger LoggerType
glType = lens _glType (\s a -> s {_glType = a})

-- | The level of the logs.
glLevel :: Lens' GreengrassLogger LoggerLevel
glLevel = lens _glLevel (\s a -> s {_glLevel = a})

-- | A descriptive or arbitrary ID for the logger. This value must be unique within the logger definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
glId :: Lens' GreengrassLogger Text
glId = lens _glId (\s a -> s {_glId = a})

-- | The component that will be subject to logging.
glComponent :: Lens' GreengrassLogger LoggerComponent
glComponent = lens _glComponent (\s a -> s {_glComponent = a})

instance FromJSON GreengrassLogger where
  parseJSON =
    withObject
      "GreengrassLogger"
      ( \x ->
          GreengrassLogger'
            <$> (x .:? "Space")
            <*> (x .: "Type")
            <*> (x .: "Level")
            <*> (x .: "Id")
            <*> (x .: "Component")
      )

instance Hashable GreengrassLogger

instance NFData GreengrassLogger

instance ToJSON GreengrassLogger where
  toJSON GreengrassLogger' {..} =
    object
      ( catMaybes
          [ ("Space" .=) <$> _glSpace,
            Just ("Type" .= _glType),
            Just ("Level" .= _glLevel),
            Just ("Id" .= _glId),
            Just ("Component" .= _glComponent)
          ]
      )
