{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EndpointFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EndpointFilter where

import Network.AWS.Comprehend.Types.EndpointStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The filter used to determine which endpoints are returned. You can filter jobs on their name, model, status, or the date and time that they were created. You can only set one filter at a time.
--
--
--
-- /See:/ 'endpointFilter' smart constructor.
data EndpointFilter = EndpointFilter'
  { _efStatus ::
      !(Maybe EndpointStatus),
    _efModelARN :: !(Maybe Text),
    _efCreationTimeAfter :: !(Maybe POSIX),
    _efCreationTimeBefore :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EndpointFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'efStatus' - Specifies the status of the endpoint being returned. Possible values are: Creating, Ready, Updating, Deleting, Failed.
--
-- * 'efModelARN' - The Amazon Resource Number (ARN) of the model to which the endpoint is attached.
--
-- * 'efCreationTimeAfter' - Specifies a date after which the returned endpoint or endpoints were created.
--
-- * 'efCreationTimeBefore' - Specifies a date before which the returned endpoint or endpoints were created.
endpointFilter ::
  EndpointFilter
endpointFilter =
  EndpointFilter'
    { _efStatus = Nothing,
      _efModelARN = Nothing,
      _efCreationTimeAfter = Nothing,
      _efCreationTimeBefore = Nothing
    }

-- | Specifies the status of the endpoint being returned. Possible values are: Creating, Ready, Updating, Deleting, Failed.
efStatus :: Lens' EndpointFilter (Maybe EndpointStatus)
efStatus = lens _efStatus (\s a -> s {_efStatus = a})

-- | The Amazon Resource Number (ARN) of the model to which the endpoint is attached.
efModelARN :: Lens' EndpointFilter (Maybe Text)
efModelARN = lens _efModelARN (\s a -> s {_efModelARN = a})

-- | Specifies a date after which the returned endpoint or endpoints were created.
efCreationTimeAfter :: Lens' EndpointFilter (Maybe UTCTime)
efCreationTimeAfter = lens _efCreationTimeAfter (\s a -> s {_efCreationTimeAfter = a}) . mapping _Time

-- | Specifies a date before which the returned endpoint or endpoints were created.
efCreationTimeBefore :: Lens' EndpointFilter (Maybe UTCTime)
efCreationTimeBefore = lens _efCreationTimeBefore (\s a -> s {_efCreationTimeBefore = a}) . mapping _Time

instance Hashable EndpointFilter

instance NFData EndpointFilter

instance ToJSON EndpointFilter where
  toJSON EndpointFilter' {..} =
    object
      ( catMaybes
          [ ("Status" .=) <$> _efStatus,
            ("ModelArn" .=) <$> _efModelARN,
            ("CreationTimeAfter" .=) <$> _efCreationTimeAfter,
            ("CreationTimeBefore" .=) <$> _efCreationTimeBefore
          ]
      )
