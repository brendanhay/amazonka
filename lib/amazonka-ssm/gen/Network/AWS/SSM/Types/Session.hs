{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.Session
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.Session where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.SessionManagerOutputURL
import Network.AWS.SSM.Types.SessionStatus

-- | Information about a Session Manager connection to an instance.
--
--
--
-- /See:/ 'session' smart constructor.
data Session = Session'
  { _sesStatus :: !(Maybe SessionStatus),
    _sesOutputURL :: !(Maybe SessionManagerOutputURL),
    _sesDocumentName :: !(Maybe Text),
    _sesEndDate :: !(Maybe POSIX),
    _sesOwner :: !(Maybe Text),
    _sesStartDate :: !(Maybe POSIX),
    _sesDetails :: !(Maybe Text),
    _sesSessionId :: !(Maybe Text),
    _sesTarget :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Session' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sesStatus' - The status of the session. For example, "Connected" or "Terminated".
--
-- * 'sesOutputURL' - Reserved for future use.
--
-- * 'sesDocumentName' - The name of the Session Manager SSM document used to define the parameters and plugin settings for the session. For example, @SSM-SessionManagerRunShell@ .
--
-- * 'sesEndDate' - The date and time, in ISO-8601 Extended format, when the session was terminated.
--
-- * 'sesOwner' - The ID of the AWS user account that started the session.
--
-- * 'sesStartDate' - The date and time, in ISO-8601 Extended format, when the session began.
--
-- * 'sesDetails' - Reserved for future use.
--
-- * 'sesSessionId' - The ID of the session.
--
-- * 'sesTarget' - The instance that the Session Manager session connected to.
session ::
  Session
session =
  Session'
    { _sesStatus = Nothing,
      _sesOutputURL = Nothing,
      _sesDocumentName = Nothing,
      _sesEndDate = Nothing,
      _sesOwner = Nothing,
      _sesStartDate = Nothing,
      _sesDetails = Nothing,
      _sesSessionId = Nothing,
      _sesTarget = Nothing
    }

-- | The status of the session. For example, "Connected" or "Terminated".
sesStatus :: Lens' Session (Maybe SessionStatus)
sesStatus = lens _sesStatus (\s a -> s {_sesStatus = a})

-- | Reserved for future use.
sesOutputURL :: Lens' Session (Maybe SessionManagerOutputURL)
sesOutputURL = lens _sesOutputURL (\s a -> s {_sesOutputURL = a})

-- | The name of the Session Manager SSM document used to define the parameters and plugin settings for the session. For example, @SSM-SessionManagerRunShell@ .
sesDocumentName :: Lens' Session (Maybe Text)
sesDocumentName = lens _sesDocumentName (\s a -> s {_sesDocumentName = a})

-- | The date and time, in ISO-8601 Extended format, when the session was terminated.
sesEndDate :: Lens' Session (Maybe UTCTime)
sesEndDate = lens _sesEndDate (\s a -> s {_sesEndDate = a}) . mapping _Time

-- | The ID of the AWS user account that started the session.
sesOwner :: Lens' Session (Maybe Text)
sesOwner = lens _sesOwner (\s a -> s {_sesOwner = a})

-- | The date and time, in ISO-8601 Extended format, when the session began.
sesStartDate :: Lens' Session (Maybe UTCTime)
sesStartDate = lens _sesStartDate (\s a -> s {_sesStartDate = a}) . mapping _Time

-- | Reserved for future use.
sesDetails :: Lens' Session (Maybe Text)
sesDetails = lens _sesDetails (\s a -> s {_sesDetails = a})

-- | The ID of the session.
sesSessionId :: Lens' Session (Maybe Text)
sesSessionId = lens _sesSessionId (\s a -> s {_sesSessionId = a})

-- | The instance that the Session Manager session connected to.
sesTarget :: Lens' Session (Maybe Text)
sesTarget = lens _sesTarget (\s a -> s {_sesTarget = a})

instance FromJSON Session where
  parseJSON =
    withObject
      "Session"
      ( \x ->
          Session'
            <$> (x .:? "Status")
            <*> (x .:? "OutputUrl")
            <*> (x .:? "DocumentName")
            <*> (x .:? "EndDate")
            <*> (x .:? "Owner")
            <*> (x .:? "StartDate")
            <*> (x .:? "Details")
            <*> (x .:? "SessionId")
            <*> (x .:? "Target")
      )

instance Hashable Session

instance NFData Session
