{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new trigger.
module Network.AWS.Glue.CreateTrigger
  ( -- * Creating a Request
    createTrigger,
    CreateTrigger,

    -- * Request Lenses
    ctWorkflowName,
    ctSchedule,
    ctPredicate,
    ctStartOnCreation,
    ctDescription,
    ctTags,
    ctName,
    ctType,
    ctActions,

    -- * Destructuring the Response
    createTriggerResponse,
    CreateTriggerResponse,

    -- * Response Lenses
    ctrsName,
    ctrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createTrigger' smart constructor.
data CreateTrigger = CreateTrigger'
  { _ctWorkflowName ::
      !(Maybe Text),
    _ctSchedule :: !(Maybe Text),
    _ctPredicate :: !(Maybe Predicate),
    _ctStartOnCreation :: !(Maybe Bool),
    _ctDescription :: !(Maybe Text),
    _ctTags :: !(Maybe (Map Text (Text))),
    _ctName :: !Text,
    _ctType :: !TriggerType,
    _ctActions :: ![Action]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTrigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctWorkflowName' - The name of the workflow associated with the trigger.
--
-- * 'ctSchedule' - A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ . This field is required when the trigger type is SCHEDULED.
--
-- * 'ctPredicate' - A predicate to specify when the new trigger should fire. This field is required when the trigger type is @CONDITIONAL@ .
--
-- * 'ctStartOnCreation' - Set to @true@ to start @SCHEDULED@ and @CONDITIONAL@ triggers when created. True is not supported for @ON_DEMAND@ triggers.
--
-- * 'ctDescription' - A description of the new trigger.
--
-- * 'ctTags' - The tags to use with this trigger. You may use tags to limit access to the trigger. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
--
-- * 'ctName' - The name of the trigger.
--
-- * 'ctType' - The type of the new trigger.
--
-- * 'ctActions' - The actions initiated by this trigger when it fires.
createTrigger ::
  -- | 'ctName'
  Text ->
  -- | 'ctType'
  TriggerType ->
  CreateTrigger
createTrigger pName_ pType_ =
  CreateTrigger'
    { _ctWorkflowName = Nothing,
      _ctSchedule = Nothing,
      _ctPredicate = Nothing,
      _ctStartOnCreation = Nothing,
      _ctDescription = Nothing,
      _ctTags = Nothing,
      _ctName = pName_,
      _ctType = pType_,
      _ctActions = mempty
    }

-- | The name of the workflow associated with the trigger.
ctWorkflowName :: Lens' CreateTrigger (Maybe Text)
ctWorkflowName = lens _ctWorkflowName (\s a -> s {_ctWorkflowName = a})

-- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ . This field is required when the trigger type is SCHEDULED.
ctSchedule :: Lens' CreateTrigger (Maybe Text)
ctSchedule = lens _ctSchedule (\s a -> s {_ctSchedule = a})

-- | A predicate to specify when the new trigger should fire. This field is required when the trigger type is @CONDITIONAL@ .
ctPredicate :: Lens' CreateTrigger (Maybe Predicate)
ctPredicate = lens _ctPredicate (\s a -> s {_ctPredicate = a})

-- | Set to @true@ to start @SCHEDULED@ and @CONDITIONAL@ triggers when created. True is not supported for @ON_DEMAND@ triggers.
ctStartOnCreation :: Lens' CreateTrigger (Maybe Bool)
ctStartOnCreation = lens _ctStartOnCreation (\s a -> s {_ctStartOnCreation = a})

-- | A description of the new trigger.
ctDescription :: Lens' CreateTrigger (Maybe Text)
ctDescription = lens _ctDescription (\s a -> s {_ctDescription = a})

-- | The tags to use with this trigger. You may use tags to limit access to the trigger. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
ctTags :: Lens' CreateTrigger (HashMap Text (Text))
ctTags = lens _ctTags (\s a -> s {_ctTags = a}) . _Default . _Map

-- | The name of the trigger.
ctName :: Lens' CreateTrigger Text
ctName = lens _ctName (\s a -> s {_ctName = a})

-- | The type of the new trigger.
ctType :: Lens' CreateTrigger TriggerType
ctType = lens _ctType (\s a -> s {_ctType = a})

-- | The actions initiated by this trigger when it fires.
ctActions :: Lens' CreateTrigger [Action]
ctActions = lens _ctActions (\s a -> s {_ctActions = a}) . _Coerce

instance AWSRequest CreateTrigger where
  type Rs CreateTrigger = CreateTriggerResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          CreateTriggerResponse' <$> (x .?> "Name") <*> (pure (fromEnum s))
      )

instance Hashable CreateTrigger

instance NFData CreateTrigger

instance ToHeaders CreateTrigger where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.CreateTrigger" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateTrigger where
  toJSON CreateTrigger' {..} =
    object
      ( catMaybes
          [ ("WorkflowName" .=) <$> _ctWorkflowName,
            ("Schedule" .=) <$> _ctSchedule,
            ("Predicate" .=) <$> _ctPredicate,
            ("StartOnCreation" .=) <$> _ctStartOnCreation,
            ("Description" .=) <$> _ctDescription,
            ("Tags" .=) <$> _ctTags,
            Just ("Name" .= _ctName),
            Just ("Type" .= _ctType),
            Just ("Actions" .= _ctActions)
          ]
      )

instance ToPath CreateTrigger where
  toPath = const "/"

instance ToQuery CreateTrigger where
  toQuery = const mempty

-- | /See:/ 'createTriggerResponse' smart constructor.
data CreateTriggerResponse = CreateTriggerResponse'
  { _ctrsName ::
      !(Maybe Text),
    _ctrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTriggerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctrsName' - The name of the trigger.
--
-- * 'ctrsResponseStatus' - -- | The response status code.
createTriggerResponse ::
  -- | 'ctrsResponseStatus'
  Int ->
  CreateTriggerResponse
createTriggerResponse pResponseStatus_ =
  CreateTriggerResponse'
    { _ctrsName = Nothing,
      _ctrsResponseStatus = pResponseStatus_
    }

-- | The name of the trigger.
ctrsName :: Lens' CreateTriggerResponse (Maybe Text)
ctrsName = lens _ctrsName (\s a -> s {_ctrsName = a})

-- | -- | The response status code.
ctrsResponseStatus :: Lens' CreateTriggerResponse Int
ctrsResponseStatus = lens _ctrsResponseStatus (\s a -> s {_ctrsResponseStatus = a})

instance NFData CreateTriggerResponse
