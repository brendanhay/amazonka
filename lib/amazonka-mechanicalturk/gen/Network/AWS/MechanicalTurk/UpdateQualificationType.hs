{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.UpdateQualificationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @UpdateQualificationType@ operation modifies the attributes of an existing Qualification type, which is represented by a QualificationType data structure. Only the owner of a Qualification type can modify its attributes.
--
--
-- Most attributes of a Qualification type can be changed after the type has been created. However, the Name and Keywords fields cannot be modified. The RetryDelayInSeconds parameter can be modified or added to change the delay or to enable retries, but RetryDelayInSeconds cannot be used to disable retries.
--
-- You can use this operation to update the test for a Qualification type. The test is updated based on the values specified for the Test, TestDurationInSeconds and AnswerKey parameters. All three parameters specify the updated test. If you are updating the test for a type, you must specify the Test and TestDurationInSeconds parameters. The AnswerKey parameter is optional; omitting it specifies that the updated test does not have an answer key.
--
-- If you omit the Test parameter, the test for the Qualification type is unchanged. There is no way to remove a test from a Qualification type that has one. If the type already has a test, you cannot update it to be AutoGranted. If the Qualification type does not have a test and one is provided by an update, the type will henceforth have a test.
--
-- If you want to update the test duration or answer key for an existing test without changing the questions, you must specify a Test parameter with the original questions, along with the updated values.
--
-- If you provide an updated Test but no AnswerKey, the new test will not have an answer key. Requests for such Qualifications must be granted manually.
--
-- You can also update the AutoGranted and AutoGrantedValue attributes of the Qualification type.
--
module Network.AWS.MechanicalTurk.UpdateQualificationType
    (
    -- * Creating a Request
      updateQualificationType
    , UpdateQualificationType
    -- * Request Lenses
    , uqtTestDurationInSeconds
    , uqtQualificationTypeStatus
    , uqtAnswerKey
    , uqtTest
    , uqtAutoGranted
    , uqtAutoGrantedValue
    , uqtDescription
    , uqtRetryDelayInSeconds
    , uqtQualificationTypeId

    -- * Destructuring the Response
    , updateQualificationTypeResponse
    , UpdateQualificationTypeResponse
    -- * Response Lenses
    , uqtrsQualificationType
    , uqtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateQualificationType' smart constructor.
data UpdateQualificationType = UpdateQualificationType'
  { _uqtTestDurationInSeconds   :: !(Maybe Integer)
  , _uqtQualificationTypeStatus :: !(Maybe QualificationTypeStatus)
  , _uqtAnswerKey               :: !(Maybe Text)
  , _uqtTest                    :: !(Maybe Text)
  , _uqtAutoGranted             :: !(Maybe Bool)
  , _uqtAutoGrantedValue        :: !(Maybe Int)
  , _uqtDescription             :: !(Maybe Text)
  , _uqtRetryDelayInSeconds     :: !(Maybe Integer)
  , _uqtQualificationTypeId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateQualificationType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uqtTestDurationInSeconds' - The number of seconds the Worker has to complete the Qualification test, starting from the time the Worker requests the Qualification.
--
-- * 'uqtQualificationTypeStatus' - The new status of the Qualification type - Active | Inactive
--
-- * 'uqtAnswerKey' - The answers to the Qualification test specified in the Test parameter, in the form of an AnswerKey data structure.
--
-- * 'uqtTest' - The questions for the Qualification test a Worker must answer correctly to obtain a Qualification of this type. If this parameter is specified, @TestDurationInSeconds@ must also be specified. Constraints: Must not be longer than 65535 bytes. Must be a QuestionForm data structure. This parameter cannot be specified if AutoGranted is true. Constraints: None. If not specified, the Worker may request the Qualification without answering any questions.
--
-- * 'uqtAutoGranted' - Specifies whether requests for the Qualification type are granted immediately, without prompting the Worker with a Qualification test. Constraints: If the Test parameter is specified, this parameter cannot be true.
--
-- * 'uqtAutoGrantedValue' - The Qualification value to use for automatically granted Qualifications. This parameter is used only if the AutoGranted parameter is true.
--
-- * 'uqtDescription' - The new description of the Qualification type.
--
-- * 'uqtRetryDelayInSeconds' - The amount of time, in seconds, that Workers must wait after requesting a Qualification of the specified Qualification type before they can retry the Qualification request. It is not possible to disable retries for a Qualification type after it has been created with retries enabled. If you want to disable retries, you must dispose of the existing retry-enabled Qualification type using DisposeQualificationType and then create a new Qualification type with retries disabled using CreateQualificationType.
--
-- * 'uqtQualificationTypeId' - The ID of the Qualification type to update.
updateQualificationType
    :: Text -- ^ 'uqtQualificationTypeId'
    -> UpdateQualificationType
updateQualificationType pQualificationTypeId_ =
  UpdateQualificationType'
    { _uqtTestDurationInSeconds = Nothing
    , _uqtQualificationTypeStatus = Nothing
    , _uqtAnswerKey = Nothing
    , _uqtTest = Nothing
    , _uqtAutoGranted = Nothing
    , _uqtAutoGrantedValue = Nothing
    , _uqtDescription = Nothing
    , _uqtRetryDelayInSeconds = Nothing
    , _uqtQualificationTypeId = pQualificationTypeId_
    }


-- | The number of seconds the Worker has to complete the Qualification test, starting from the time the Worker requests the Qualification.
uqtTestDurationInSeconds :: Lens' UpdateQualificationType (Maybe Integer)
uqtTestDurationInSeconds = lens _uqtTestDurationInSeconds (\ s a -> s{_uqtTestDurationInSeconds = a})

-- | The new status of the Qualification type - Active | Inactive
uqtQualificationTypeStatus :: Lens' UpdateQualificationType (Maybe QualificationTypeStatus)
uqtQualificationTypeStatus = lens _uqtQualificationTypeStatus (\ s a -> s{_uqtQualificationTypeStatus = a})

-- | The answers to the Qualification test specified in the Test parameter, in the form of an AnswerKey data structure.
uqtAnswerKey :: Lens' UpdateQualificationType (Maybe Text)
uqtAnswerKey = lens _uqtAnswerKey (\ s a -> s{_uqtAnswerKey = a})

-- | The questions for the Qualification test a Worker must answer correctly to obtain a Qualification of this type. If this parameter is specified, @TestDurationInSeconds@ must also be specified. Constraints: Must not be longer than 65535 bytes. Must be a QuestionForm data structure. This parameter cannot be specified if AutoGranted is true. Constraints: None. If not specified, the Worker may request the Qualification without answering any questions.
uqtTest :: Lens' UpdateQualificationType (Maybe Text)
uqtTest = lens _uqtTest (\ s a -> s{_uqtTest = a})

-- | Specifies whether requests for the Qualification type are granted immediately, without prompting the Worker with a Qualification test. Constraints: If the Test parameter is specified, this parameter cannot be true.
uqtAutoGranted :: Lens' UpdateQualificationType (Maybe Bool)
uqtAutoGranted = lens _uqtAutoGranted (\ s a -> s{_uqtAutoGranted = a})

-- | The Qualification value to use for automatically granted Qualifications. This parameter is used only if the AutoGranted parameter is true.
uqtAutoGrantedValue :: Lens' UpdateQualificationType (Maybe Int)
uqtAutoGrantedValue = lens _uqtAutoGrantedValue (\ s a -> s{_uqtAutoGrantedValue = a})

-- | The new description of the Qualification type.
uqtDescription :: Lens' UpdateQualificationType (Maybe Text)
uqtDescription = lens _uqtDescription (\ s a -> s{_uqtDescription = a})

-- | The amount of time, in seconds, that Workers must wait after requesting a Qualification of the specified Qualification type before they can retry the Qualification request. It is not possible to disable retries for a Qualification type after it has been created with retries enabled. If you want to disable retries, you must dispose of the existing retry-enabled Qualification type using DisposeQualificationType and then create a new Qualification type with retries disabled using CreateQualificationType.
uqtRetryDelayInSeconds :: Lens' UpdateQualificationType (Maybe Integer)
uqtRetryDelayInSeconds = lens _uqtRetryDelayInSeconds (\ s a -> s{_uqtRetryDelayInSeconds = a})

-- | The ID of the Qualification type to update.
uqtQualificationTypeId :: Lens' UpdateQualificationType Text
uqtQualificationTypeId = lens _uqtQualificationTypeId (\ s a -> s{_uqtQualificationTypeId = a})

instance AWSRequest UpdateQualificationType where
        type Rs UpdateQualificationType =
             UpdateQualificationTypeResponse
        request = postJSON mechanicalTurk
        response
          = receiveJSON
              (\ s h x ->
                 UpdateQualificationTypeResponse' <$>
                   (x .?> "QualificationType") <*> (pure (fromEnum s)))

instance Hashable UpdateQualificationType where

instance NFData UpdateQualificationType where

instance ToHeaders UpdateQualificationType where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.UpdateQualificationType"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateQualificationType where
        toJSON UpdateQualificationType'{..}
          = object
              (catMaybes
                 [("TestDurationInSeconds" .=) <$>
                    _uqtTestDurationInSeconds,
                  ("QualificationTypeStatus" .=) <$>
                    _uqtQualificationTypeStatus,
                  ("AnswerKey" .=) <$> _uqtAnswerKey,
                  ("Test" .=) <$> _uqtTest,
                  ("AutoGranted" .=) <$> _uqtAutoGranted,
                  ("AutoGrantedValue" .=) <$> _uqtAutoGrantedValue,
                  ("Description" .=) <$> _uqtDescription,
                  ("RetryDelayInSeconds" .=) <$>
                    _uqtRetryDelayInSeconds,
                  Just
                    ("QualificationTypeId" .= _uqtQualificationTypeId)])

instance ToPath UpdateQualificationType where
        toPath = const "/"

instance ToQuery UpdateQualificationType where
        toQuery = const mempty

-- | /See:/ 'updateQualificationTypeResponse' smart constructor.
data UpdateQualificationTypeResponse = UpdateQualificationTypeResponse'
  { _uqtrsQualificationType :: !(Maybe QualificationType)
  , _uqtrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateQualificationTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uqtrsQualificationType' - Contains a QualificationType data structure.
--
-- * 'uqtrsResponseStatus' - -- | The response status code.
updateQualificationTypeResponse
    :: Int -- ^ 'uqtrsResponseStatus'
    -> UpdateQualificationTypeResponse
updateQualificationTypeResponse pResponseStatus_ =
  UpdateQualificationTypeResponse'
    {_uqtrsQualificationType = Nothing, _uqtrsResponseStatus = pResponseStatus_}


-- | Contains a QualificationType data structure.
uqtrsQualificationType :: Lens' UpdateQualificationTypeResponse (Maybe QualificationType)
uqtrsQualificationType = lens _uqtrsQualificationType (\ s a -> s{_uqtrsQualificationType = a})

-- | -- | The response status code.
uqtrsResponseStatus :: Lens' UpdateQualificationTypeResponse Int
uqtrsResponseStatus = lens _uqtrsResponseStatus (\ s a -> s{_uqtrsResponseStatus = a})

instance NFData UpdateQualificationTypeResponse where
